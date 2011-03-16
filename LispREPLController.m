//
//  LispREPLController.m
//  Moxie
//
//  Created by Brian Cully on Mon Aug 09 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "LispREPLController.h"

enum repl_lock_condition { NO_DATA, HAS_DATA };

@implementation LispREPLController
+ (LispREPLController *)sharedController
{
    static LispREPLController *sharedController = nil;
    
    if (sharedController == nil) {
        sharedController = [[LispREPLController alloc] init];
    }
    return sharedController;
}

- (id)init
{
    self = [self initWithWindowNibName: @"LispREPL"];
    if (self) {
        theHistoryLevel = 0;
        waitingForResult = YES;
        [self setWindowFrameAutosaveName: @"lispREPLWindow"];
        // Make sure the window is there, so we get all the output even if it isn't open.
        [self window];

    }
    return self;
}

- (void)dealloc
{
    NSNotificationCenter *defaultCenter;
    
    defaultCenter = [NSNotificationCenter defaultCenter];
    [defaultCenter removeObserver: self];
    
    [theUpdates release];
    [theREPLUpdateLock release];
    
    [super dealloc];
}

- (void)scrollToEnd: (NSTextView *)aTextView
{
    NSScroller *verticalScroller;
    NSScrollView *myScrollView;
    
    myScrollView = (NSScrollView *)[[aTextView superview] superview];
    verticalScroller = [myScrollView verticalScroller];
    [aTextView scrollRangeToVisible: NSMakeRange([[aTextView textStorage] length], 0)];    
//    if ([verticalScroller isEnabled] == NO || (1.0 - [verticalScroller floatValue]) < 0.000001)
//        [aTextView scrollRangeToVisible: NSMakeRange([[aTextView textStorage] length], 0)];    
}

// Add a newline, print the prompt, and set the mark to the end of the buffer.
- (void)updateREPLFont
{
    NSTextStorage *ts;
    NSUserDefaults *defaults;
    
    defaults = [NSUserDefaults standardUserDefaults];
    ts = [[self textView] textStorage];
    [ts addAttribute: NSFontAttributeName
               value: [defaults REPLFont]
               range: NSMakeRange(0, [ts length])];
}

// TODO:
// Optimize redraw by caching previous values and checking for deltas.
// Unfortunately, the notification posted for changes doesn't include said deltas.
- (void)redisplay
{
    NSRect fontRect;
    NSTextView *tv;
    NSUserDefaults *defaults;
    
    defaults = [NSUserDefaults standardUserDefaults];
    tv = [self textView];
    [tv setBackgroundColor: [defaults REPLBackgroundColor]];
    [tv setInsertionPointColor: [defaults REPLInputTextColor]];
    [tv setTypingAttributes: [self REPLInputAttributes]];
    [self updateREPLFont];
    
    fontRect = [[defaults REPLFont] boundingRectForFont];
    if (fontRect.size.height > 0) {
        [[self window] setContentResizeIncrements:
            NSMakeSize(fontRect.size.width, fontRect.size.height)];
    }
}

- (void)windowDidLoad
{
    NSNotificationCenter *defaultCenter;
    
    [super windowDidLoad];
    [[self window] setOpaque: NO];

    [[LispREPL sharedREPL] addCommand: @":REPL-RESULT"
                              handler: self
                             selector: @selector(setLispResult:)];
    [[LispREPL sharedREPL] addCommand: @":REPL-DBG"
                              handler: self
                             selector: @selector(lispInDebugger:)];
    [[LispREPL sharedREPL] addCommand: @":REGISTER-KEYSTROKE"
                              handler: self
                             selector: @selector(registerKeystrokeMacro:)];
    [[LispREPL sharedREPL] addCommand: @":UNREGISTER-KEYSTROKE"
                              handler: self
                             selector: @selector(unregisterKeystrokeMacro:)];
    
    theREPLUpdateLock = [[NSConditionLock alloc] initWithCondition: NO_DATA];
    theUpdates = [[NSMutableArray alloc] init];
    [NSThread detachNewThreadSelector: @selector(readREPLData:)
                             toTarget: self
                           withObject: [[LispREPL sharedREPL] stdoutReader]];
    
    [LispREPL sharedREPL];
    
    theREPLUpdateTimer = [NSTimer scheduledTimerWithTimeInterval: 0.1
                                                          target: self
                                                        selector: @selector(scanForREPLData:)
                                                        userInfo: nil
                                                         repeats: YES];

    defaultCenter = [NSNotificationCenter defaultCenter];
    [defaultCenter addObserver: self
                      selector: @selector(preferencesChanged:)
                          name: NSUserDefaultsDidChangeNotification
                        object: [NSUserDefaults standardUserDefaults]];
    
    [self redisplay];
}

- (LispSymbol *)keystrokeFromEvent: (NSEvent *)anEvent
{
    LispSymbol *rc;
    
    rc = nil;
    if ([anEvent type] == NSKeyDown) {
        NSString *characters;
        NSString *modifier, *keycode;
        unsigned int i;
        
        modifier = @"";
        if ([anEvent modifierFlags] & NSCommandKeyMask) {
            modifier = [modifier stringByAppendingString: @"CMD-"];
        }
        if ([anEvent modifierFlags] & NSAlternateKeyMask) {
            modifier = [modifier stringByAppendingString: @"OPT-"];
        }
        if ([anEvent modifierFlags] & NSControlKeyMask) {
            modifier = [modifier stringByAppendingString: @"CTRL-"];
        }
        if ([anEvent modifierFlags] & NSShiftKeyMask) {
            modifier = [modifier stringByAppendingString: @"SHIFT-"];
        }
        if ([anEvent modifierFlags] & NSNumericPadKeyMask) {
            modifier = [modifier stringByAppendingString: @"NUMPAD-"];
        }
        
        // This sucks. We should be encoding things in a structure, not a keyword.
        keycode = @"";
        characters = [anEvent charactersIgnoringModifiers];
        for (i = 0; i < [characters length]; i++) {
            switch ([characters characterAtIndex: i]) {
                case '(':
                    keycode = @"LPAREN";
                    break;
                case ')':
                    keycode = @"RPAREN";
                    break;
                case ':':
                    keycode = @"COLON";
                    break;
                case '|':
                    keycode = @"PIPE";
                    break;
                case ';':
                    keycode = @"SEMICOLON";
                    break;
                case '\\':
                    keycode = @"BACKSLASH";
                    break;
                case '\'':
                    keycode = @"QUOTE";
                    break;
                case '`':
                    keycode = @"BACKQUOTE";
                    break;
                case '\t':
                    keycode = @"TAB";
                    break;
                case 27:
                    keycode = @"ESC";
                    break;
                case 127:
                    keycode = @"BACKSPACE";
                    break;
                case 3:
                case 13:
                    keycode = @"RETURN";
                    break;
                case NSUpArrowFunctionKey:
                    keycode = @"UP";
                    break;
                case NSDownArrowFunctionKey:
                    keycode = @"DOWN";
                    break;
                case NSLeftArrowFunctionKey:
                    keycode = @"LEFT";
                    break;
                case NSRightArrowFunctionKey:
                    keycode = @"RIGHT";
                    break;
                case NSF1FunctionKey:
                    keycode = @"F1";
                    break;
                case NSF2FunctionKey:
                    keycode = @"F2";
                    break;
                case NSF3FunctionKey:
                    keycode = @"F3";
                    break;
                case NSF4FunctionKey:
                    keycode = @"F4";
                    break;
                case NSF5FunctionKey:
                    keycode = @"F5";
                    break;
                case NSF6FunctionKey:
                    keycode = @"F6";
                    break;
                case NSF7FunctionKey:
                    keycode = @"F7";
                    break;
                case NSF8FunctionKey:
                    keycode = @"F8";
                    break;
                case NSF9FunctionKey:
                    keycode = @"F9";
                    break;
                case NSF10FunctionKey:
                    keycode = @"F10";
                    break;
                case NSF11FunctionKey:
                    keycode = @"F11";
                    break;
                case NSF12FunctionKey:
                    keycode = @"F12";
                    break;
                case NSF13FunctionKey:
                    keycode = @"F13";
                    break;
                case NSF14FunctionKey:
                    keycode = @"F14";
                    break;
                case NSF15FunctionKey:
                    keycode = @"F15";
                    break;
                case NSF16FunctionKey:
                    keycode = @"F16";
                    break;
                case NSF17FunctionKey:
                    keycode = @"F17";
                    break;
                case NSF18FunctionKey:
                    keycode = @"F18";
                    break;
                case NSF19FunctionKey:
                    keycode = @"F19";
                    break;
                case NSF20FunctionKey:
                    keycode = @"F20";
                    break;
                case NSF21FunctionKey:
                    keycode = @"F21";
                    break;
                case NSF22FunctionKey:
                    keycode = @"F22";
                    break;
                case NSF23FunctionKey:
                    keycode = @"F23";
                    break;
                case NSF24FunctionKey:
                    keycode = @"F24";
                    break;
                case NSF25FunctionKey:
                    keycode = @"F25";
                    break;
                case NSF26FunctionKey:
                    keycode = @"F26";
                    break;
                case NSF27FunctionKey:
                    keycode = @"F27";
                    break;
                case NSF28FunctionKey:
                    keycode = @"F28";
                    break;
                case NSF29FunctionKey:
                    keycode = @"F29";
                    break;
                case NSF30FunctionKey:
                    keycode = @"F30";
                    break;
                case NSF31FunctionKey:
                    keycode = @"F31";
                    break;
                case NSF32FunctionKey:
                    keycode = @"F32";
                    break;
                case NSF33FunctionKey:
                    keycode = @"F33";
                    break;
                case NSF34FunctionKey:
                    keycode = @"F34";
                    break;
                case NSF35FunctionKey:
                    keycode = @"F35";
                    break;
                case NSInsertFunctionKey:
                    keycode = @"INS";
                    break;
                case NSDeleteFunctionKey:
                    keycode = @"DEL";
                    break;
                case NSHomeFunctionKey:
                    keycode = @"HOME";
                    break;
                case NSBeginFunctionKey:
                    keycode = @"BEGIN";
                    break;
                case NSEndFunctionKey:
                    keycode = @"END";
                    break;
                case NSPageUpFunctionKey:
                    keycode = @"PAGEUP";
                    break;
                case NSPageDownFunctionKey:
                    keycode = @"PAGEDOWN";
                    break;
                case NSClearLineFunctionKey:
                    keycode = @"CLEAR";
                    break;
                default:
                    keycode = [NSString stringWithFormat: @"%c", [characters characterAtIndex: i]];
            }
        }
        rc = [LispSymbol symbolNamed: [NSString stringWithFormat: @":%@%@", modifier, keycode]];
    }
    NSLog(@"DEBUG: sending keycode: %@", rc);
    return rc;
}

- (BOOL)dispatchKeystrokeMacro: (NSEvent *)anEvent
                        fromID: (NSNumber *)anID
{
    LispSymbol *keystroke;
    BOOL rc;
    
    rc = NO;
    keystroke = [self keystrokeFromEvent: anEvent];
    if (keystroke) {
        if ([[self keystrokeMacros] containsObject: keystroke]) {
            [[LispREPL sharedREPL] eval:
                [NSString stringWithFormat: @"(moxie::world-event %@ :keystroke-macro-hook %@)",
                    anID, [keystroke lispForm]]];
            rc = YES;
        }
    }
    return rc;
}

- (void)registerKeystrokeMacro: (NSArray *)form
{
    LispSymbol *keystroke;
    
    keystroke = [form objectAtIndex: 0];
    [[self keystrokeMacros] addObject: keystroke];
}

- (void)unregisterKeystrokeMacro: (NSArray *)form
{
    LispSymbol *keystroke;
    
    keystroke = [form objectAtIndex: 0];
    [[self keystrokeMacros] removeObject: keystroke];
}

- (void)goBackwardInHistory
{
    if (theHistoryLevel > 0) {
        NSMutableString *buffer;
        
        buffer = [[[self textView] textStorage] mutableString];
        if (NO) {
            [[self inputHistory] addObject: [buffer substringWithRange: NSMakeRange(0, [buffer length])]];
            theInputViewIsDirty = NO;
            theHistoryLevel = [[self inputHistory] count] - 1;
        }
        theHistoryLevel--;

        [buffer replaceCharactersInRange: NSMakeRange([self mark],
                                                      [buffer length] - [self mark])
                              withString: [[self inputHistory] objectAtIndex: theHistoryLevel]];
        [self scrollToEnd: [self textView]];
    }
}

- (void)goForwardInHistory
{
    if (theHistoryLevel < [[self inputHistory] count]-1) {
        NSMutableString *buffer;
        
        buffer = [[[self textView] textStorage] mutableString];
        if (theInputViewIsDirty) {
            [[self inputHistory] addObject: [buffer substringWithRange: NSMakeRange(0, [buffer length])]];
            theInputViewIsDirty = NO;
            theHistoryLevel = [[self inputHistory] count];
        } else
            theHistoryLevel++;
        
        [buffer replaceCharactersInRange: NSMakeRange([self mark],
                                                      [buffer length] - [self mark])
                              withString: [[self inputHistory] objectAtIndex: theHistoryLevel]];
        [self scrollToEnd: [self textView]];
    }
}

- (void)textDidChange: (NSNotification *)aNotification
{
    if ([aNotification object] == theTextView) {
        // Flag the input view as changed.
        theInputViewIsDirty = YES;
    }
}

// XXX - this needs to change badly. We need to do full form
// parsing here or figure out a way around it entirely.
//
// This can probably tie into the indentation system.
- (BOOL)isValidLispForm: (NSString *)aForm
{
    int parenCount;
    int i, formLen;
    
    formLen = [aForm length];
    parenCount = 0;
    for (i = 0; i < formLen; i++) {
        switch ([aForm characterAtIndex: i]) {
            case '(':
                parenCount++;
                break;
            case ')':
                parenCount--;
                break;
        }
    }

    return parenCount == 0;
}

- (void)sendInputToLisp: (NSString *)aForm
{
    int formLen;
    
    // Get the new text, which is between the old mark and the end of the buffer.
    formLen = [aForm length];
    if (formLen > 0) {
        NSMutableString *buffer;
        NSTextStorage *ts;
        
        // Save line in history.
        if (theInputViewIsDirty || theHistoryLevel < [[self inputHistory] count]-1) {
            [[self inputHistory] addObject: [aForm substringWithRange: NSMakeRange(0, formLen)]];
            theInputViewIsDirty = NO;
        }
        theHistoryLevel = [[self inputHistory] count];

        // Update the marks when we send data to lisp.
        ts = [[self textView] textStorage];
        buffer = [[[self textView] textStorage] mutableString];
        [self setMark: [buffer length]];
        [self setValueMark: [buffer length]];
        
        if (waitingForResult)
            [[LispREPL sharedREPL] eval: aForm];
        else
            [self sendEvent: @":eval" withArg: [LispSymbol symbolNamed: aForm]]; // XXX: This is a cheap and dirty way to avoid getting
                                                                                 // the string quoted. We should just parse out the tree
                                                                                 // and send that array.
        waitingForResult = YES;
    }
}

- (void)setLispResult: (NSArray *)alist
{
    NSArray *result;
    NSDictionary *resultDict;
    NSMutableString *resultStr;
    NSMutableString *buffer;
    NSTextStorage *ts;
    NSString *prompt;

    ts = [[self textView] textStorage];
    buffer = [[[self textView] textStorage] mutableString];

    resultDict = [NSDictionary dictionaryWithAlist: alist];
    prompt = [[resultDict objectForKey: @":PROMPT"] objectAtIndex: 0];
    result = [resultDict objectForKey: @":VALUES"];

    if (result) {
        resultStr = [NSMutableString stringWithString: @"=>"];
        if ([result isKindOfClass: [NSArray class]]) {
            NSEnumerator *resultEnum;
            id obj;
            
            resultEnum = [result objectEnumerator];
            while ((obj = [resultEnum nextObject]) != nil)
                [resultStr appendFormat: @" %@", [obj lispForm]];
        } else
            [resultStr appendFormat: @" %@", [result lispForm]];
        
        [resultStr appendString: @"\n"];
        
        [buffer appendString: resultStr];
        [ts addAttributes: [self REPLReturnValueAttributes]
                    range: NSMakeRange([self valueMark], [resultStr length])];
    }
    [self setMark: [buffer length]];

    if (prompt) {
        waitingForResult = NO;
        
        [buffer appendString: prompt];
        [ts addAttributes: [self REPLPromptAttributes]
                    range: NSMakeRange([self mark], [prompt length])];
    }
    [self setMark: [buffer length]];

    [ts addAttributes: [self REPLInputAttributes]
                range: NSMakeRange([self mark] - 1, 1)];
    [[self textView] setTypingAttributes: [self REPLInputAttributes]];
    
    [self scrollToEnd: [self textView]];
}

/*
 * When we're in the debugger, let it use its own prompt. And in the event that we've already printed our
 * prompt, delete it.
 */
- (void)lispInDebugger: (NSArray *)args
{
    NSMutableString *buffer;
    NSRange range;
    
    waitingForResult = YES;
    
    buffer = [[[self textView] textStorage] mutableString];
    range = NSMakeRange([self valueMark], [self mark] - [self valueMark]);
    [buffer deleteCharactersInRange: range];
    [self setMark: [self mark] - range.length];
}

- (void)scanForREPLData: (NSDictionary *)userInfo
{
    [theREPLUpdateLock lock];
    while ([theUpdates count] > 0) {
        NSAttributedString *replString;
        NSData *updateData;
		NSString *updateStr;
        NSTextStorage *ts;

        updateData = [theUpdates objectAtIndex: 0];
		updateStr = [[[NSString alloc] initWithBytes: [updateData bytes]
											  length: [updateData length]
											encoding: NSUTF8StringEncoding] autorelease];
        replString = [NSAttributedString attributedStringWithString: updateStr
                                                         attributes: [self REPLOutputAttributes]];
        
        // Insert before values, and update marks.
        ts = [[self textView] textStorage];
        [ts insertAttributedString: replString atIndex: [self valueMark]];
        [self setMark: [self mark] + [replString length]];
        [self setValueMark: [self valueMark] + [replString length]];
        
        [self scrollToEnd: [self textView]];
        [theUpdates removeObjectAtIndex: 0];
    }
    [theREPLUpdateLock unlock];
}

- (void)readREPLData: (NSFileHandle *)stdoutInput
{
    while (1) {
        NSAutoreleasePool *rp;
        NSData *inputData;
        
        rp = [[NSAutoreleasePool alloc] init];
        inputData = [stdoutInput availableData];
        if ([inputData length] == 0)
            continue;
        [theREPLUpdateLock lock];
        [theUpdates addObject: inputData];
        [theREPLUpdateLock unlock];
        [rp release];
    }
}

- (void)preferencesChanged: (NSNotification *)aNotification
{
    [self redisplay];
}

- (void)changeFont: (id)sender
{
    NSDictionary *attrs;
    NSFont *oldFont, *newFont;
    
    attrs = [[self textView] typingAttributes];
    oldFont = [attrs objectForKey: NSFontAttributeName];
    newFont = [sender convertFont: oldFont];
    
    [[NSUserDefaults standardUserDefaults] setREPLFontData:
        [NSArchiver archivedDataWithRootObject: newFont]];
}

- (BOOL)textView: (NSTextView *)aTextView
shouldChangeTextInRange: (NSRange)affectedCharRange
    replacementString: (NSString *)replacementString
{
    if (replacementString && affectedCharRange.location < (unsigned)[self mark]) {
        NSBeep();
        [[self textView] setSelectedRange: NSMakeRange([self mark], 0)];
        [self scrollToEnd: [self textView]];
        return NO;
    }
    return YES;
}

- (NSArray *)textView: (NSTextView *)textView
          completions: (NSArray *)words
  forPartialWordRange: (NSRange)charRange
  indexOfSelectedItem: (int *)i
{
    NSLog(@"[LispREPLController textView %@ completions: %@ forPartialWordRange: (%u, %u) indexOfSelectedItem: %d]",
          textView, words, (unsigned)charRange.location, (unsigned)charRange.length, *i);
    return words;
}

- (void)sendEvent: (NSString *)anEvent
{
    [self sendEventWithArgs: anEvent, nil];
}

- (void)sendEvent: (NSString *)anEvent withArg: (id)anArg
{
    [self sendEventWithArgs: anEvent, anArg, nil];
}

-(void)sendEventWithArgs: (NSString *)anEvent, ...
{
    va_list ap;
    
    va_start(ap, anEvent);
    [self sendEvent: anEvent arguments: ap];
    va_end(ap);
}

-(void)sendEvent: (NSString *)anEvent arguments: (va_list)args
{
    NSMutableArray *argArray;
    id arg;

    argArray = [NSMutableArray arrayWithObject: [LispSymbol symbolNamed: anEvent]];
    while ((arg = va_arg(args, id)) != nil) {
        [argArray addObject: arg];
    }
    [[LispREPL sharedREPL] eval: [NSString stringWithFormat: @"(apply #'moxie::moxie-event-handler '%@)",
        [argArray lispForm]]];
}

- (NSNumber *)handleEvent: (NSEvent *)anEvent from: (id)sender
{
    BOOL rc;
    
    rc = NO;
    if ([anEvent type] == NSKeyDown) {
        NSMutableString *buffer;
        NSString *characters;
        unsigned int i;
        
        buffer = [[[self textView] textStorage] mutableString];
        characters = [anEvent charactersIgnoringModifiers];
        for (i = 0; i < [characters length]; i++) {
            switch ([characters characterAtIndex: i]) {
                case '\r':
                case '\n': {
                    NSString *changedBuffer;
                    
                    changedBuffer = [buffer substringFromIndex: [self mark]];
                    if ([self isValidLispForm: changedBuffer]) {
                        [buffer appendString: @"\n"];
                        [self sendInputToLisp: changedBuffer];
                        
                        [[self textView] setSelectedRange: NSMakeRange([buffer length], 0)];
                        rc = YES;
                    }
                    break;
                }
                case NSUpArrowFunctionKey:
                    if ([anEvent modifierFlags] & NSCommandKeyMask) {
                        [self goBackwardInHistory];
                        rc = YES;
                    }
                    break;
                case NSDownArrowFunctionKey:
                    if ([anEvent modifierFlags] & NSCommandKeyMask) {
                        [self goForwardInHistory];
                        rc = YES;
                    }
                    break;
            }
        }
    }
    
    return [NSNumber numberWithBool: rc];
}
@end

@implementation LispREPLController (TextAttributes)
- (NSDictionary *)REPLPromptAttributes
{
    NSMutableDictionary *myAttrs;
    NSUserDefaults *defaults;
    
    defaults = [NSUserDefaults standardUserDefaults];
    myAttrs = [NSMutableDictionary dictionary];
    [myAttrs setObject: [defaults REPLFont] forKey: NSFontAttributeName];
    [myAttrs setObject: [defaults REPLPromptColor] forKey: NSForegroundColorAttributeName];
    [myAttrs setObject: @"prompt" forKey: @"REPL.style"];
    
    return myAttrs;
}

- (NSDictionary *)REPLInputAttributes
{
    NSMutableDictionary *myAttrs;
    NSUserDefaults *defaults;
    
    defaults = [NSUserDefaults standardUserDefaults];
    myAttrs = [NSMutableDictionary dictionary];
    [myAttrs setObject: [defaults REPLFont] forKey: NSFontAttributeName];
    [myAttrs setObject: [defaults REPLInputTextColor] forKey: NSForegroundColorAttributeName];
    [myAttrs setObject: @"input" forKey: @"REPL.style"];
    
    return myAttrs;
}

- (NSDictionary *)REPLOutputAttributes
{
    NSMutableDictionary *myAttrs;
    NSUserDefaults *defaults;
    
    defaults = [NSUserDefaults standardUserDefaults];
    myAttrs = [NSMutableDictionary dictionary];
    [myAttrs setObject: [defaults REPLFont] forKey: NSFontAttributeName];
    [myAttrs setObject: [defaults REPLOutputTextColor] forKey: NSForegroundColorAttributeName];
    [myAttrs setObject: @"output" forKey: @"REPL.style"];
    
    return myAttrs;
}

- (NSDictionary *)REPLReturnValueAttributes
{
    NSMutableDictionary *myAttrs;
    NSUserDefaults *defaults;
    
    defaults = [NSUserDefaults standardUserDefaults];
    myAttrs = [NSMutableDictionary dictionary];
    [myAttrs setObject: [defaults REPLFont] forKey: NSFontAttributeName];
    [myAttrs setObject: [defaults REPLReturnValueColor] forKey: NSForegroundColorAttributeName];
    [myAttrs setObject: @"result" forKey: @"REPL.style"];
    
    return myAttrs;
}
@end

@implementation LispREPLController (Accessors)
- (NSTextView *)textView
{
    return theTextView;
}

- (int)mark
{
    return theMark;
}

- (void)setMark: (int)aPoint
{
    theMark = aPoint;
}

- (NSMutableSet *)keystrokeMacros
{
    if (theKeystrokeMacros == nil)
        [self setKeystrokeMacros: [NSMutableSet set]];
    return theKeystrokeMacros;
}

- (void)setKeystrokeMacros: (NSMutableSet *)macros
{
    [macros retain];
    [theKeystrokeMacros release];
    theKeystrokeMacros = macros;
}

- (NSMutableArray *)inputHistory
{
    if (theInputHistory == nil)
        [self setInputHistory: [NSMutableArray array]];
    return theInputHistory;
}

- (int)valueMark
{
    return theValueMark;
}

- (void)setValueMark: (int)aPoint
{
    theValueMark = aPoint;
}

- (void)setInputHistory: (NSMutableArray *)aHistory
{
    [aHistory retain];
    [theInputHistory release];
    theInputHistory = aHistory;
}
@end