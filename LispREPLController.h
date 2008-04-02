//
//  LispREPLController.h
//  Moxie
//
//  Created by Brian Cully on Mon Aug 09 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "LispREPL.h"

@interface LispREPLController : NSWindowController
{
    IBOutlet NSTextView *theTextView;
    
    int theMark, theValueMark;
    BOOL theInputViewIsDirty;
    
    NSTimer *theREPLUpdateTimer;
    NSConditionLock *theREPLUpdateLock;
    NSMutableArray *theUpdates;
    BOOL waitingForResult;
    
    NSMutableSet *theKeystrokeMacros;
    
    NSMutableArray *theInputHistory;
    unsigned theHistoryLevel;
}

+ (LispREPLController *)sharedController;

- (void)sendEvent: (NSString *)anEvent;
- (void)sendEvent: (NSString *)anEvent withArg: (id)anArg;
- (void)sendEventWithArgs: (NSString *)anEvent, ...;
- (void)sendEvent: (NSString *)anEvent arguments: (va_list)args;

- (BOOL)dispatchKeystrokeMacro: (NSEvent *)anEvent
                        fromID: (NSNumber *)aWorld;
@end

@interface LispREPLController (TextAttributes)
- (NSDictionary *)REPLPromptAttributes;
- (NSDictionary *)REPLInputAttributes;
- (NSDictionary *)REPLOutputAttributes;
- (NSDictionary *)REPLReturnValueAttributes;
@end

@interface LispREPLController (Accessors)
- (NSTextView *)textView;

- (int)mark;
- (void)setMark: (int)aPoint;
- (int)valueMark;
- (void)setValueMark: (int)aPoint;

- (NSMutableSet *)keystrokeMacros;
- (void)setKeystrokeMacros: (NSMutableSet *)macros;

- (NSMutableArray *)inputHistory;
- (void)setInputHistory: (NSMutableArray *)aHistory;
@end