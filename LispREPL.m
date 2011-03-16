//
//  LispREPL.m
//  Moxie
//
//  Created by Brian Cully on Sat Aug 14 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "LispREPL.h"
#import "LispREPLController.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

enum repl_lock_condition { NO_DATA, HAS_DATA };

@implementation LispREPL
void
sig_pipe(int signo)
{
	NSLog(@"WARNING: SIGPIPE caught.");
}

void
sig_child(int signo)
{
	int status;
    
	if (wait(&status) == -1) {
		NSLog(@"WARNING: Couldn't clean up child: %s.\n", strerror(errno));
		return;
	}
}

+ (LispREPL *)sharedREPL
{
    static LispREPL *sharedREPL;
    
    if (sharedREPL == nil)
        sharedREPL = [[self alloc] init];
    return sharedREPL;
}

- (int)initLispProcess;
{
    NSString *helperPath;
    NSString *resLocation, *pluginLocation, *frameworkLocation;
    int pin[2], pout[2], res[2];
    pid_t pid;
    struct sigaction sa, ocsa, opsa;

    helperPath = [[NSBundle mainBundle] pathForAuxiliaryExecutable: @"startlisp"];
    if (helperPath == nil) {
        NSLog(@"Couldn't find helper application in resources!");
        return -1;
    }

    resLocation = [[NSBundle mainBundle] resourcePath];
    pluginLocation = [[NSBundle mainBundle] builtInPlugInsPath];
    frameworkLocation = [[NSBundle mainBundle] privateFrameworksPath];
    
    if (pipe(pin) == -1 || pipe(pout) == -1 || pipe(res) == -1) {
        NSLog(@"Couldn't create pipes for REPL!");
        return -1;
    }

    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = sig_child;
    sigaction(SIGCHLD, &sa, &ocsa);
    sa.sa_handler = sig_pipe;
    sigaction(SIGPIPE, &sa, &opsa);

    setsid();
    pid = fork();
    if (pid == -1) {
        NSLog(@"ERROR: Couldn't fork off lisp process: %s.", strerror(errno));
        return -1;
    } else if (pid == 0) {
        sa.sa_flags = 0;
        sigemptyset(&sa.sa_mask);
        sa.sa_handler = SIG_DFL;
        sigaction(SIGCHLD, &sa, NULL);
        sigaction(SIGPIPE, &sa, NULL);
        
        close(pin[1]); close(pout[0]); close(res[0]);
        if (dup2(pin[0], STDIN_FILENO) == -1 ||
            dup2(pout[1], STDOUT_FILENO) == -1 ||
            dup2(pout[1], STDERR_FILENO) == -1 ||
            dup2(res[1], STDERR_FILENO+1) == -1) {
            NSLog(@"ERROR: Couldn't setup standard I/O pipes: %s.", strerror(errno));
            exit(1);
        }
        execl([helperPath UTF8String], [[helperPath lastPathComponent] UTF8String],
              [resLocation UTF8String], [pluginLocation UTF8String], [frameworkLocation UTF8String], NULL);
        exit(1);
    }
    
    close(pin[0]); close(pout[1]); close(res[1]);
    theStdinWriter = [[NSFileHandle alloc] initWithFileDescriptor: pin[1]
                                                   closeOnDealloc: YES];
    theStdoutReader = [[NSFileHandle alloc] initWithFileDescriptor: pout[0]
                                                    closeOnDealloc: YES];
    theResultReader = [[NSFileHandle alloc] initWithFileDescriptor: res[0]
                                                    closeOnDealloc: YES];
        
    return 0;
}

- (id)init
{
    self = [super init];
    if (self) {
        [self initLispProcess];
        
        // Set up the result reader thread.
        theREPLResults = [[NSMutableArray alloc] init];
        theCommandHandlers = [[NSMutableDictionary alloc] init];
        theDispatcherLock = [[NSConditionLock alloc] initWithCondition: NO_DATA];
        theLispIsLoaded = NO;
        [NSThread detachNewThreadSelector: @selector(readREPLResults:)
                                 toTarget: self
                               withObject: [self resultReader]];
        theDispatcherTimer = [NSTimer scheduledTimerWithTimeInterval: 0.1
                                                              target: self
                                                            selector: @selector(dispatchREPLResults:)
                                                            userInfo: nil
                                                             repeats: YES];
    }
    return self;
}

- (void)dealloc
{
    // Gotta kill the reader thread here.
    [theDispatcherLock release];
    [theDispatcherTimer release];
    
    // Then close file handles.
    [[self stdinWriter] release];
    [[self stdoutReader] release];
    [[self resultReader] release];
    
    // Then release our data.
    [theREPLResults release];
    [theCommandHandlers release];
    [super dealloc];
}

- (BOOL)isLoaded
{
    return theLispIsLoaded;
}

- (NSArray *)handlerInfoForCommand: (id)command
{
    NSArray *handlerInfo;

    handlerInfo = [[self commandHandlers] objectForKey: [command isKindOfClass: [NSString class]] ? command : [command stringValue]];
    if (handlerInfo && [handlerInfo count] == 2)
        return handlerInfo;
    return nil;
}

- (void)setHandlerInfo: (NSArray *)handlerInfo forCommand: (NSString *)command
{
    if ([handlerInfo count] == 2)
        [[self commandHandlers] setObject: handlerInfo forKey: command];
    else if (handlerInfo == nil)
        [[self commandHandlers] removeObjectForKey: command];
}

- (void)addCommand: (NSString *)command
           handler: (id)object
          selector: (SEL)handler
{
    [self setHandlerInfo: [NSArray arrayWithObjects: object, NSStringFromSelector(handler), nil]
              forCommand: command];
}

- (void)removeCommand: (NSString *)command
{
    [self setHandlerInfo: nil forCommand: command];
}

- (void)runCommand: (NSString *)command withObjects: (id)objects
{
    id handler;
    SEL selector;

    handler = [self handlerForCommand: command];
    if (handler) {
        selector = [self selectorForCommand: command];

        NS_DURING
            [handler performSelector: selector withObject: objects];
        NS_HANDLER
            NSLog(@"WARNING: Got exception: %@ (%@) while evaluating %@:%@",
                  [localException name], [localException reason], handler, NSStringFromSelector(selector));
        NS_ENDHANDLER
    }
}

- (BOOL)commandHasHandler: (NSString *)command
{
    if ([self handlerInfoForCommand: command])
        return YES;
    return NO;
}

- (id)handlerForCommand: (NSString *)command
{
    return [[self handlerInfoForCommand: command] objectAtIndex: 0];
}

- (SEL)selectorForCommand: (NSString *)command
{
    return NSSelectorFromString([[self handlerInfoForCommand: command] objectAtIndex: 1]);
}

- (void)eval: (id)aForm
{
    [[self stdinWriter] writeData: [[NSString stringWithFormat: @"%@\n", aForm]
        dataUsingEncoding: NSUTF8StringEncoding]];
}

/*
 * Read and parse result forms from the REPL. Once parsed into arrays, dispatch them via
 * the main thread by adding them to a worker queue.
 */
- (void)readREPLResults: (NSFileHandle *)resultsHandle
{
    NS_DURING
        while (1) {
            NSAutoreleasePool *rp;
            id results;
            
            rp = [[NSAutoreleasePool alloc] init];
            results = [resultsHandle readLispForm];
            [theDispatcherLock lock];
            [theREPLResults addObject: results];
            [theDispatcherLock unlock];
            [rp release];
        }
    NS_HANDLER
        [self performSelectorOnMainThread: @selector(replDiedSelector:)
                               withObject: self
                            waitUntilDone: NO];
    NS_ENDHANDLER
}

- (void)replDiedSelector: (id)sender
{
    [[LispREPLController sharedController] showWindow: self];
    NSRunCriticalAlertPanel(@"REPL died", @"The REPL sub-process has died, and Moxie cannot continue.",
                            @"Quit", nil, nil);
    [NSApp stop: self]; 
}

/*
 * Wait for updates on the queue and dispatch results to the appropriate callback
 * method on the values, if they're there, and call regular callbacks on the rest of
 * the elements.
 */
- (void)dispatchREPLResults: (NSNotification *)resultsNotification
{
    [theDispatcherLock lock];
    if (theLispIsLoaded == NO) {
        NSNotificationCenter *defaultCenter;
        theLispIsLoaded = YES;
        
        defaultCenter = [NSNotificationCenter defaultCenter];
        [defaultCenter postNotificationName: LispFinishedLoadingNotification
                                     object: self];
    }

    while ([theREPLResults count] > 0) {
        id result;
        
        result = [theREPLResults objectAtIndex: 0];
        if ([result isKindOfClass: [NSArray class]]) {
            NSString *command;
            NSArray *argArray;
            
            command = [result objectAtIndex: 0];
            argArray = [result subarrayWithRange: NSMakeRange(1, [result count] - 1)];
            [self runCommand: command withObjects: argArray];
        } else {
            NSLog(@"ERROR: Couldn't dispatch results: %@", result);
        }
        [theREPLResults removeObjectAtIndex: 0];
    }
    [theDispatcherLock unlock];
}
@end

@implementation LispREPL (Accessors)
- (NSMutableDictionary *)commandHandlers
{
    return theCommandHandlers;
}

- (NSFileHandle *)stdinWriter
{
    return theStdinWriter;
}

- (NSFileHandle *)stdoutReader
{
    return theStdoutReader;
}

- (NSFileHandle *)resultReader
{
    return theResultReader;
}
@end