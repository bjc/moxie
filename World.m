//
//  World.m
//  Moxie
//
//  Created by Brian Cully on Wed Dec 24 2003.
//  Copyright (c) 2003 Brian Cully. All rights reserved.
//

#import "World.h"

#import "WorldSettingsController.h"
#import "WorldStatusController.h"

#import <objc/objc-runtime.h>

#include <unistd.h>

NSString *
localizedSettingName(NSString *settingName)
{
    NSString *localeString;
    
    localeString = NSLocalizedStringFromTable(settingName, @"SettingNames", nil);
    if (localeString)
        return localeString;
    return settingName;
}

@implementation World
- (id)init
{
    self = [super init];
    if (self) {
        [self setIdentifier: [LispSymbol symbolNamed: [NSString stringWithFormat: @":COCOA-ID-%u", (unsigned int)self]]];
        theHistoryLevel = 0;
        [self setStatus: MxDisconnected];
    }
    return self;
}

- (void)dealloc
{
    [self setInputHistory: nil];
    [self setSettings: nil];
    [self setTimer: nil];

    [super dealloc];
}

- (NSString *)windowNibName
{
    return @"World";
}

- (void)windowControllerDidLoadNib: (NSWindowController *)aController
{
    [super windowControllerDidLoadNib: aController];

    [self setHasUndoManager: YES];
    [[self undoManager] removeAllActions];
    [theProgressIndicator setUsesThreadedAnimation: YES];

    [[self window] setOpaque: NO];
    [[self window] useOptimizedDrawing: NO];        // Text views overlap split views.
}

- (NSString *)displayName
{
    return [[super displayName] stringByDeletingPathExtension];
}

- (BOOL)readFromURL: (NSURL *)absoluteURL ofType: (NSString *)aType error: (NSError **)errorPtr
{
    NSLog(@"DEBUG: [World readFromURL: %@ ofType: %@ error: (ptr)]", absoluteURL, aType);

    [self sendWindowEventWithArgs: @":LOAD-WORLD", [LispSymbol symbolNamed: @":PATH"], [absoluteURL path], nil];
    return YES;
}

- (BOOL)writeToURL: (NSURL *)absoluteURL ofType: (NSString *)aType error: (NSError **)errorPtr
{
    NSLog(@"DEBUG: [World writeToURL: %@ ofType: %@ error: (ptr)]", absoluteURL, aType);

    [self sendWindowEventWithArgs: @":SAVE-WORLD", [LispSymbol symbolNamed: @":PATH"], [absoluteURL path], nil];
    usleep(2000000); // XXX - should just wait until the file shows up or something.
    return YES;
}

- (BOOL)isDocumentEdited
{
    BOOL edited;
    
    edited = [self status] != MxDisconnected;
    [[self window] setDocumentEdited: edited];          // We need to do this because "save:" clears it.
    return edited;
}

- (void)worldConnected
{
    [self setStatus: MxConnected];
    [theConnectButton setTitle: @"Disconnect"];
    [theConnectButton setAction: @selector(close:)];
}

- (void)worldDisconnected
{
    [self setStatus: MxDisconnected];
    [theConnectButton setTitle: @"Connect"];
    [theConnectButton setAction: @selector(open:)];
}

- (void)worldClosed
{
    NSLog(@"DEBUG: %@ should close world.", [[self identifier] stringValue]);
}

- (void)textDidChange: (NSNotification *)aNotification
{
    if ([aNotification object] == theInputView) {
        // Flag the input view as changed.
        theInputViewIsDirty = YES;
    }
}

- (BOOL)validateMenuItem: (NSMenuItem *)anItem
{
    if ([anItem action] == @selector(pasteWithFormatting:)) {
        NSPasteboard *pb;
        
        pb = [NSPasteboard generalPasteboard];
        if ([[pb types] containsObject: NSStringPboardType])
            return YES;
        else
            return NO;
    }
    return [super validateMenuItem: anItem];
}

- (IBAction)open: (id)sender
{
    [self sendWindowEvent: @":connect-world"];
}

- (IBAction)close: (id)sender
{
    [self sendWindowEvent: @":disconnect-world"];
}

- (void)startProgressBar
{
    [theProgressIndicator startAnimation: self];
}

- (void)stopProgressBar
{
    if ([[LispREPL sharedREPL] isLoaded])
        [theProgressIndicator stopAnimation: self];
}

- (NSString *)statusBuffer
{
    return [theRoomField stringValue];
}

// XXX - there appears to be a race condition here with 
// theRoomField not being available.
- (void)setStatusBuffer: (NSString *)aString
{
    if ([self queuedRoomString])
        [self setQueuedRoomString: aString];
    else if (aString)
        [theRoomField setStringValue: aString];
}

- (id)identifier
{
    return theID;
}

- (void)setIdentifier: (id)anID
{
    [[LispREPL sharedREPL] removeCommand: [anID stringValue]];
    [[LispREPL sharedREPL] addCommand: [anID stringValue]
                              handler: self
                             selector: @selector(windowEventFinished:)];
    [anID retain];
    [theID release];
    theID = anID;
}

- (NSDictionary *)defaultTextAttributes
{
    NSMutableDictionary *myAttrs;
    
    myAttrs = [NSMutableDictionary dictionary];
    if ([[self settings] font])
        [myAttrs setObject: [[self settings] font] forKey: NSFontAttributeName];
    if ([[self settings] textColor])
        [myAttrs setObject: [[self settings] textColor] forKey: NSForegroundColorAttributeName];
    if ([[self settings] backgroundColor])
        [myAttrs setObject: [[self settings] backgroundColor] forKey: NSBackgroundColorAttributeName];
    
    return myAttrs;
}

- (void)redisplay
{
    NSMutableDictionary *myAttrs;
    
    myAttrs = [[NSMutableDictionary alloc] init];
    if ([[self settings] font])
        [myAttrs setObject: [[self settings] font] forKey: NSFontAttributeName];
    if ([[self settings] textColor])
        [myAttrs setObject: [[self settings] textColor] forKey: NSForegroundColorAttributeName];

    [[theOutputView textStorage] setAttributes: myAttrs
                                         range: NSMakeRange(0, [[self outputBuffer] length])];
    [myAttrs release];
}

- (void)windowWillClose: (id) sender
{
    [self setTimer: nil];
    [[NSNotificationCenter defaultCenter] removeObserver: self];
    [self sendWindowEvent: @":CLOSE-WORLD"];
    [[LispREPL sharedREPL] removeCommand: [[self identifier] stringValue]];
}

- (void)windowDidBecomeKey: (NSNotification *)aNotification
{
    [[aNotification object] makeFirstResponder: theInputView];
}

- (void)windowDidResize: (NSNotification *)aNotification
{
    NSWindow *window;
    NSRect scaledWindowFrame, fontRect;

    fontRect = [[[self settings] font] boundingRectForFont];

    window = [aNotification object];
    scaledWindowFrame = [window frame];
    scaledWindowFrame.size.width = round(scaledWindowFrame.size.width / fontRect.size.width);
    scaledWindowFrame.size.height = round(scaledWindowFrame.size.height / fontRect.size.height);

    [[self settings] setWindowFrame: scaledWindowFrame];
}

- (void)scrollToEnd: (NSTextView *)aTextView
{
    NSScroller *verticalScroller;
    NSScrollView *myScrollView;
    
    myScrollView = (NSScrollView *)[[aTextView superview] superview];
    verticalScroller = [myScrollView verticalScroller];
    if ([verticalScroller isEnabled] == NO || (1.0 - [verticalScroller floatValue]) < 0.000001)
        [aTextView scrollRangeToVisible: NSMakeRange([[aTextView textStorage] length], 0)];    
}

- (void)splitViewDidResizeSubviews: (NSNotification *)aNotification
{
    NSSize fontSize, inputSize;
    float scaledHeight;

    fontSize = [[[self settings] font] boundingRectForFont].size;
    inputSize = [theInputView frame].size;
    scaledHeight = round(inputSize.height / fontSize.height);
    [[self settings] setInputViewSize: scaledHeight];
}

- (float)splitView: (NSSplitView *)sender constrainMinCoordinate: (float)proposedMin ofSubviewAt: (int)offset
{
    NSRect fontRect;
    float fontHeight;

    NSLog(@"DEBUG: splitView: sender constrainMinCoordinate: %f ofSubviewAt: %d", proposedMin, offset);
    fontRect = [[[self settings] font] boundingRectForFont];
    fontHeight = fontRect.size.height - fontRect.origin.y;
    NSLog(@"DEBUG: returning %f.", fontHeight);
    
    return fontHeight;
}

- (float)splitView: (NSSplitView *)sender constrainMaxCoordinate: (float)proposedMax ofSubviewAt: (int)offset
{
    NSRect fontRect;
    float fontHeight;
    
    NSLog(@"DEBUG: splitView: sender constrainMaxCoordinate: %f ofSubviewAt: %d", proposedMax, offset);
    fontRect = [[[self settings] font] boundingRectForFont];
    fontHeight = fontRect.size.height - fontRect.origin.y;
    fontHeight *= round(proposedMax / fontHeight);
    NSLog(@"DEBUG: returning %f.", fontHeight);
    
    return fontHeight;
}

- (float)splitView: (NSSplitView *)sender
constrainSplitPosition: (float)proposedPosition
       ofSubviewAt: (int)offset
{
    NSRect fontRect;
    
    fontRect = [[[self settings] font] boundingRectForFont];
    if (fontRect.size.height > 0) {
        float myPos, fontHeight;
        int numLines;

        fontHeight = fontRect.size.height;
        
        numLines = ceil(proposedPosition / fontHeight);
        myPos = numLines * fontHeight - fontHeight / 2;
        return floor(myPos + 0.5);
    }
    return proposedPosition;
}

- (void)splitView: (NSSplitView *)sender resizeSubviewsWithOldSize: (NSSize)oldSize
{
    NSView *topView, *bottomView;
    NSRect newFrame, outputFrame, inputFrame, fontRect;
    float dividerThickness;

    // Split view is flipped.
    topView = [[theSplitView subviews] objectAtIndex: 0];
    bottomView = [[theSplitView subviews] objectAtIndex: 1];
    dividerThickness = [sender dividerThickness];
    newFrame = [sender frame];
    
    inputFrame = [bottomView frame];
    outputFrame = [topView frame];

/*
    inputFrame.origin.y = newFrame.size.height;
    inputFrame.size.width = newFrame.size.width;
 */
    inputFrame.size.height = [[self settings] inputViewSize];
    fontRect = [[[self settings] font] boundingRectForFont];
    if (fontRect.size.height > 0)
        inputFrame.size.height *= fontRect.size.height;
    inputFrame.origin.y = newFrame.size.height - inputFrame.size.height;
    inputFrame.size.width = newFrame.size.width;

    outputFrame.size.width = newFrame.size.width;
    outputFrame.size.height = inputFrame.origin.y - dividerThickness - outputFrame.origin.y;

    [topView setFrame: outputFrame];
    [bottomView setFrame: inputFrame];
    
    [theOutputView scrollRangeToVisible: NSMakeRange([[theOutputView textStorage] length], 0)];    
}

- (void)goBackwardInHistory
{
    if (theHistoryLevel > 0) {
        NSMutableString *buffer;
        
        buffer = [self inputBuffer];
        if (theInputViewIsDirty) {
            [[self inputHistory] addObject: [buffer substringWithRange: NSMakeRange(0, [buffer length])]];
            theInputViewIsDirty = NO;
            theHistoryLevel = [[self inputHistory] count] - 1;
        }
        theHistoryLevel--;
        [buffer setString: [[self inputHistory] objectAtIndex: theHistoryLevel]];
        [[theInputView textStorage] setAttributes: [self defaultTextAttributes]
                                            range: NSMakeRange(0, [buffer length])];
    }
}

- (void)goForwardInHistory
{
    if (theHistoryLevel < [[self inputHistory] count]-1) {
        NSMutableString *buffer;
        
        buffer = [self inputBuffer];
        if (theInputViewIsDirty) {
            [[self inputHistory] addObject: [buffer substringWithRange: NSMakeRange(0, [buffer length])]];
            theInputViewIsDirty = NO;
            theHistoryLevel = [[self inputHistory] count];
        } else
            theHistoryLevel++;
        
        [buffer setString: [[self inputHistory] objectAtIndex: theHistoryLevel]];
        [[theInputView textStorage] setAttributes: [self defaultTextAttributes]
                                            range: NSMakeRange(0, [buffer length])];
    }
}

- (void)sendInputBuffer
{
    NSMutableString *buffer;
    
    buffer = [self inputBuffer];
    if ([buffer length] > 0) {
        // Save line in history.
        if (theInputViewIsDirty || theHistoryLevel < [[self inputHistory] count]-1) {
            [[self inputHistory] addObject: [buffer substringWithRange: NSMakeRange(0, [buffer length])]];
            theInputViewIsDirty = NO;
        }
        theHistoryLevel = [[self inputHistory] count];
    }
        
    [self sendWindowEvent: @":input-from-client-hook" withArg: buffer];
    [buffer setString: @""];
}

- (void)printLispResult: (id)result;
{
    NSAttributedString *attributedResults;

    [self setStatus: MxRecentActivity];
    if ([result isKindOfClass: [NSArray class]]) {
        attributedResults = [NSAttributedString attributedStringWithForm: result
                                                       defaultAttributes: [self defaultTextAttributes]];
    } else
        attributedResults = [NSAttributedString attributedStringWithString: result
                                                                attributes: [self defaultTextAttributes]];
    [[theOutputView textStorage] insertAttributedString: attributedResults
                                                atIndex: [[self outputBuffer] length]];
    [self scrollToEnd: theOutputView];
}

- (NSNumber *)handleEvent: (NSEvent *)anEvent from: (id)sender
{
    BOOL rc;

    rc = NO;
    if ([anEvent type] == NSKeyDown) {
        if (sender == theInputView) {
            // Do this here, because it gets randomly reset sometimes.
            [theInputView setTypingAttributes: [self defaultTextAttributes]];
            if ([[LispREPLController sharedController] dispatchKeystrokeMacro: anEvent
                                                                       fromID: [self identifier]])
                rc = YES;
            else {
                NSString *characters;
                unsigned int i;
                
                characters = [anEvent charactersIgnoringModifiers];
                for (i = 0; i < [characters length]; i++) {
                    switch ([characters characterAtIndex: i]) {
                        case '\r':
                        case '\n':
                            [self sendInputBuffer];
                            rc = YES;
                            break;
                        case NSUpArrowFunctionKey:
                            if ([anEvent modifierFlags] & NSCommandKeyMask) {
                                [self goBackwardInHistory];
                            }
                            rc = YES;
                            break;
                        case NSDownArrowFunctionKey:
                            if ([anEvent modifierFlags] & NSCommandKeyMask) {
                                [self goForwardInHistory];
                            }
                            rc = YES;
                            break;
                        case NSPageUpFunctionKey:
                            [theOutputView pageUp: self];
                            rc = YES;
                            break;
                        case NSPageDownFunctionKey:
                            [theOutputView pageDown: self];
                            rc = YES;
                            break;
                    }
                }
            }
        } else {
            [[self window] makeFirstResponder: theInputView];
            [theInputView keyDown: anEvent];
            rc = YES;
        }
    }

    return [NSNumber numberWithBool: rc];
}

/*
 * Ideally, lisp would be able to query these things whenever it wanted, however,
 * that's fairly difficult to get right, so we do something a little more limited,
 * but easier.
 */
- (NSString *)lispEnvironment
{
    NSMutableDictionary *environment;
    NSString *logPath;
    
    logPath = [[[NSUserDefaults standardUserDefaults] logDirectory] stringByAppendingPathComponent: [self displayName]];
    logPath = [logPath stringByAppendingPathExtension: @"txt"];
    
    environment = [NSMutableDictionary dictionary];
    [environment setObject: [self displayName]
                    forKey: [LispSymbol symbolNamed: @":DOCUMENT-NAME"]];
    [environment setObject: logPath
                    forKey: [LispSymbol symbolNamed: @":LOG-FILE-PATH"]];
    return [environment lispForm];
}

- (void)canCloseDocumentWithDelegate: (id)delegate
                 shouldCloseSelector: (SEL)shouldCloseSelector
                         contextInfo: (void *)contextInfo
{
    if ([self isDocumentEdited]) {
        NSString *alertString, *infoString;
        
        [[self windowForSheet] makeKeyAndOrderFront: nil];
        alertString = [NSString stringWithFormat: @"Close connection to %@?", [self displayName]];
        infoString = [NSString stringWithFormat: @"Closing this window will terminate your connection to %@.",
            [self displayName]];
        closeDelegate = delegate;
        didCloseSelector = shouldCloseSelector;
        closeContext = contextInfo;
        NSBeginAlertSheet(alertString,
                          @"Close", @"Cancel", nil,
                          [self windowForSheet], self,
                          @selector(didEndCloseSheet:returnCode:contextInfo:),
                          nil, NULL, infoString);
    } else
        objc_msgSend(delegate, shouldCloseSelector, self, YES, contextInfo);
}

- (void)didEndCloseSheet: (NSWindow *)sheet
              returnCode: (int)rc
             contextInfo: (void *)context
{
    if (rc == NSAlertDefaultReturn) {
        // Close
        [self close: self];
    } else
        (void)objc_msgSend(closeDelegate, didCloseSelector, self, NO, closeContext);
}

- (IBAction)pasteWithFormatting: (id)sender
{
    NSMutableString *newString;
    NSPasteboard *pb;
    NSString *pbString;
    NSRange substringRange;
    unsigned int i;
    
    NSLog(@"DEBUG: paste with formatting");
    pb = [NSPasteboard generalPasteboard];
    pbString = [pb stringForType: NSStringPboardType];
    
    newString = [NSMutableString stringWithString: pbString];
    substringRange.location = 0;
    for (i = 0; i < [newString length]; i++) {
        switch ([newString characterAtIndex: i]) {
            case '\r':
                [newString deleteCharactersInRange: NSMakeRange(i, 1)];
                i--;
                break;
            case '\n':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%r"];
                i++;
                break;
            case '\t':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%t"];
                i++;
                break;
            case ' ':
                if ([newString characterAtIndex: i-1] == ' ') {
                    [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                             withString: @"%b"];
                    i++;
                }
                break;
            case '\\':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"\\\\"];
                i++;
                break;
            case '[':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%["];
                i++;
                break;
            case ']':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%]"];
                i++;
                break;
            case '%':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%%"];
                i++;
                break;
            case ';':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%;"];
                i++;
                break;
            case ',':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%,"];
                i++;
                break;
            case '{':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%{"];
                i++;
                break;
            case '}':
                [newString replaceCharactersInRange: NSMakeRange(i, 1)
                                         withString: @"%}"];
                i++;
                break;
        }
    }

    [theInputView insertText: newString];
}

- (void)sendWindowEvent: (NSString *)anEvent
{
    [self sendWindowEventWithArgs: anEvent, nil];
}

- (void)sendWindowEvent: (NSString *)anEvent
                withArg: (id)anArg
{
    [self sendWindowEventWithArgs: anEvent, anArg, nil];
}

-(void)sendWindowEventWithArgs: (NSString *)anEvent, ...
{
    va_list ap;

    va_start(ap, anEvent);
    [[LispREPLController sharedController] sendEvent: [NSString stringWithFormat: @":WORLD-EVENT %@ %@",
        [[self identifier] stringValue], anEvent]
                                           arguments: ap];
    va_end(ap);
}

- (void)worldSettingsChanged: (NSArray *)aList
{
    NSFont *font;
    NSMutableDictionary *settings;

    settings = [NSDictionary dictionaryWithAlist: aList];
    [self setSettings: [MxWorldSettings settingsWithDictionary: settings]];
    
    [theOutputView setTypingAttributes: [self defaultTextAttributes]];
    [theOutputView setBackgroundColor: [[self settings] backgroundColor]];
    [theInputView setBackgroundColor: [[self settings] backgroundColor]];
    [theInputView setInsertionPointColor: [[self settings] textColor]];
    [theInputView setTypingAttributes: [self defaultTextAttributes]];
    
    font = [[self settings] font];
    if (font) {
        NSRect fontRect, frameRect;

        frameRect = [[self settings] windowFrame];
        fontRect = [font boundingRectForFont];
        if (fontRect.size.height > 0) {
            [[self window] setContentResizeIncrements:
                NSMakeSize(fontRect.size.width, fontRect.size.height)];
            frameRect.size.width *= fontRect.size.width;
            frameRect.size.height *= fontRect.size.height;
        }    

        [[self window] setFrame: frameRect display: YES];
    }

    [self redisplay];
}

// The inverse of lisp's moxie::world-event.
- (void)windowEventFinished: (NSArray *)resultObjects
{
    id event;
    
    event = [resultObjects objectAtIndex: 0];
    if ([event isEqualToString: @":CLEAR-SCREEN"])
        [[self outputBuffer] setString: @""];
    else if ([event isEqualToString: @":CHANGE-SETTINGS"])
        [self worldSettingsChanged: [resultObjects objectAtIndex: 1]];
    else if ([event isEqualToString: @":OUTPUT-FROM-SERVER-HOOK"])
        [self printLispResult: [resultObjects objectAtIndex: 1]];
    else if ([event isEqualToString: @":SET-STATUS-BUFFER"])
        [self setStatusBuffer: [resultObjects objectAtIndex: 1]];
    else if ([event isEqualToString: @":ENABLE-LOGGING"])
        [[self settings] setLoggingEnabled: YES];
    else if ([event isEqualToString: @":DISABLE-LOGGING"])
        [[self settings] setLoggingEnabled: NO];
    else if ([event isEqualToString: @":WORLD-CONNECTED"])
        [self worldConnected];
    else if ([event isEqualToString: @":WORLD-DISCONNECTED"])
        [self worldDisconnected];
    else if ([event isEqualToString: @":WORLD-CLOSED"])
        [self worldClosed];
}
@end

@implementation World (Accessors)
- (NSWindow *)window
{
    return [self windowForSheet];
}

- (NSString *)queuedRoomString
{
    return theQueuedRoomString;
}

- (void)setQueuedRoomString: (NSString *)aString
{
    [aString retain];
    [theQueuedRoomString release];
    theQueuedRoomString = aString;
}

- (MxWorldStatus)status
{
    return theStatus;
}

- (void)setStatus: (MxWorldStatus)aStatus
{
    theStatus = aStatus;
    [[WorldStatusController sharedController] update];
    (void)[self isDocumentEdited];
}

- (NSMutableString *)outputBuffer
{
    return [[theOutputView textStorage] mutableString];
}

- (NSMutableString *)inputBuffer
{
    return [[theInputView textStorage] mutableString];
}

- (NSMutableArray *)inputHistory
{
    if (theInputHistory == nil)
        [self setInputHistory: [NSMutableArray array]];
    return theInputHistory;
}

- (void)setInputHistory: (NSMutableArray *)aHistory
{
    [aHistory retain];
    [theInputHistory release];
    theInputHistory = aHistory;
}

- (MxWorldSettings *)settings
{
    if (theSettings == nil)
        [self setSettings: [MxWorldSettings settingsFromDefaults]];
    return theSettings;
}

- (void)setSettings: (MxWorldSettings *)someSettings
{
    [[NSNotificationCenter defaultCenter] removeObserver: self
                                                    name: MxWorldSettingsDidChangeNotification
                                                  object: theSettings];
    [[NSNotificationCenter defaultCenter] addObserver: self
                                             selector: @selector(settingsDidChange:)
                                                 name: MxWorldSettingsDidChangeNotification
                                               object: someSettings];

    [someSettings retain];
    [theSettings release];
    theSettings = someSettings;
}

- (NSTimer *)timer
{
    return theTimer;
}

- (void)setTimer: (NSTimer *)aTimer
{
    if (aTimer != theTimer) {
        [aTimer retain];
        [theTimer invalidate];
        [theTimer release];
        theTimer = aTimer;
    }
}
@end

@implementation World (WorldSettingsDelegate)
- (void)settingsDidChange: (NSNotification *)aNotification
{
    NSString *setting;

    setting = [[aNotification userInfo] objectForKey: MxSettingName];
    if (setting) {
        id oldValue;

        [self sendWindowEvent: @":SETTING-CHANGED" withArg:
            [NSArray arrayWithObjects:
                [LispSymbol symbolNamed: setting], [[self settings] objectForKey: setting], nil]];

        if ([setting isEqualToString: @":WINDOW-SIZE"] ||
            [setting isEqualToString: @":WINDOW-ORIGIN"] ||
            [setting isEqualToString: @":INPUT-VIEW-SIZE"])
            return;
        else if ([setting isEqualToString: @":FONT"]) {
            NSRect fontRect;

            fontRect = [[[self settings] font] boundingRectForFont];
            if (fontRect.size.height > 0) {
                [[self window] setContentResizeIncrements:
                    NSMakeSize(fontRect.size.width, fontRect.size.height)];
            }
            [self redisplay];
        } else if ([setting isEqualToString: @":BACKGROUND-COLOR"]) {
            [theOutputView setBackgroundColor: [[self settings] backgroundColor]];
            [theInputView setBackgroundColor: [[self settings] backgroundColor]];
            [self redisplay];
        } else if ([setting isEqualToString: @":TEXT-COLOR"]) {
            [theInputView setInsertionPointColor: [[self settings] textColor]];
            [theInputView setTypingAttributes: [self defaultTextAttributes]];
            [self redisplay];
        }
        
        oldValue = [[aNotification userInfo] objectForKey: MxSettingOldValue];
        if (oldValue) {
            NSString *actionName;
            NSUndoManager *undoManager;
            
            undoManager = [self undoManager];
            [[undoManager prepareWithInvocationTarget: [self settings]] setObject: oldValue forKey: setting];
            
            actionName = [NSString stringWithFormat: @"Set %@", localizedSettingName(setting)];
            [undoManager setActionName: actionName];
        }
    }
}
@end

@implementation World (LineScanner)
#define TELNET_SE 240
#define TELNET_NOP 241
#define TELNET_DM 242
#define TELNET_BREAK 243
#define TELNET_IP 244
#define TELNET_AO 245
#define TELNET_AYT 246
#define TELNET_EC 247
#define TELNET_EL 248
#define TELNET_GA 249
#define TELNET_SB 250
#define TELNET_WILL 251
#define TELNET_WONT 252
#define TELNET_DO 253
#define TELNET_DONT 254
#define TELNET_IAC 255

/* These are all defined in RFCs 856 - 862 */
#define TELNET_OPT_TRANSMIT_BINARY 0

// To support this, we'll need to be able to lose the input bar.
#define TELNET_OPT_ECHO 1
#define TELNET_OPT_SUPPRESS_GO_AHEAD 3
#define TELNET_OPT_STATUS 5
#define TELNET_OPT_TIMING_MARK 6
#define TELNET_OPT_EXTENDED_OPTIONS_LIST 255

#define TELNET_OPT_STATUS_IS 0
#define TELNET_OPT_STATUS_SEND 1

// MUD extensions
#define TELNET_OPT_MCCP 86      // Mud Client Compression Protocol
#define TELNET_OPT_MSP 90       // Mud Sound Protocol
#define TELNET_OPT_MXP 91       // Mud eXtension Protocol

- (NSData *)handleTelnetCodesInData: (NSData *)someData
{
    NSMutableData *tmpData;
    const unsigned char *buffer;
    int i, lastLoc, dataLen;

    buffer = [someData bytes];
    dataLen = [someData length];
    tmpData = [NSMutableData data];
    for (lastLoc = 0, i = 0; i < dataLen; i++) {
        if (buffer[i] == TELNET_IAC) {
            NSMutableArray *telnetCodes;
            NSRange iacRange;
            
            telnetCodes = [NSMutableArray arrayWithObject: [NSNumber numberWithInt: buffer[i]]];
            [tmpData appendBytes: buffer + lastLoc
                          length: i - lastLoc];
            iacRange = NSMakeRange(i, 1);
            if (++i < dataLen) {
                iacRange.length++;
                [telnetCodes addObject: [NSNumber numberWithInt: buffer[i]]];
                switch (buffer[i]) {
                    case TELNET_WILL:
                    case TELNET_WONT:
                    case TELNET_DO:
                    case TELNET_DONT:
                        if (++i < dataLen) {
                            [telnetCodes addObject: [NSNumber numberWithInt: buffer[i]]];
                            iacRange.length++;
                        }
                        break;
                    case TELNET_SB:
                        NSLog(@"DEBUG: start of telnet block. should scan forward to TELNET_SE");
                        while (++i < (dataLen - 1)) {
                            iacRange.length++;
                            if (buffer[i] == TELNET_IAC && buffer[i+i] == TELNET_SE) {
                                [telnetCodes addObject: [NSNumber numberWithInt: buffer[i]]];
                                iacRange.length++;
                                break;
                            }
                        }
                        break;
                }
            }
            [self sendWindowEvent: @":telnet-option-hook" withArg: telnetCodes];
            lastLoc = iacRange.location + iacRange.length;
        }
    }
    if ((dataLen - lastLoc) > 0)
        [tmpData appendBytes: buffer + lastLoc
                      length: dataLen - lastLoc];
 
    return tmpData;
}
@end