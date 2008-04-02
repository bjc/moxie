//
//  LispREPL.h
//  Moxie
//
//  Created by Brian Cully on Sat Aug 14 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#define LispFinishedLoadingNotification @"LispFinishedLoadingNotification"

@interface LispREPL : NSObject
{
    NSFileHandle *theStdinWriter;
    NSFileHandle *theStdoutReader;
    NSFileHandle *theResultReader;
    
    NSMutableDictionary *theCommandHandlers;
    
    NSMutableArray *theREPLResults;
    NSTimer *theDispatcherTimer;
    NSLock *theDispatcherLock;
    
    BOOL theLispIsLoaded;
}
+ (LispREPL *)sharedREPL;

- (BOOL)isLoaded;

- (void)addCommand: (NSString *)command
           handler: (id)object
          selector: (SEL)handler;
- (void)removeCommand: (NSString *)command;
- (void)runCommand: (NSString *)command withObjects: (id)objects;
- (BOOL)commandHasHandler: (NSString *)command;
- (id)handlerForCommand: (NSString *)command;
- (SEL)selectorForCommand: (NSString *)command;

- (void)eval: (id)aForm;
@end

@interface LispREPL (Accessors)
- (NSMutableDictionary *)commandHandlers;

- (NSFileHandle *)stdinWriter;
- (NSFileHandle *)stdoutReader;
- (NSFileHandle *)resultReader;
@end