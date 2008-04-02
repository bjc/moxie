//
//  World.h
//  Moxie
//
//  Created by Brian Cully on Wed Dec 24 2003.
//  Copyright (c) 2003 Brian Cully. All rights reserved.
//


#import "LispREPLController.h"
#import "MxWorldSettings.h"

enum parsemode_t {
    DESC, LOOK, NEW, CONTENTS, EXITS, EXAMINE, EXITLIST
};

enum _statusmode_t {
    MxRecentActivity, MxConnected, MxDisconnected
};
typedef enum _statusmode_t MxWorldStatus;

@interface World : NSDocument
{
    IBOutlet ScrollingTextView *theOutputView;
    IBOutlet ScrollingTextView *theInputView;
    IBOutlet NSSplitView *theSplitView;
    IBOutlet NSProgressIndicator *theProgressIndicator;
    IBOutlet NSTextField *theRoomField;
    
    IBOutlet NSButton *theConnectButton;

    id theID;
    MxWorldStatus theStatus;
    NSString *theQueuedRoomString;
    
    NSMutableArray *theInputHistory;
    unsigned theHistoryLevel;
    BOOL theInputViewIsDirty;
    id closeDelegate;
    SEL didCloseSelector;
    void *closeContext;

    MxWorldSettings *theSettings;
    NSTimer *theTimer;
}
- (IBAction)open: (id)sender;
- (IBAction)close: (id)sender;

- (void)startProgressBar;
- (void)stopProgressBar;
- (NSString *)statusBuffer;
- (void)setStatusBuffer: (NSString *)aString;

- (id)identifier;
- (void)setIdentifier: (id)anID;

- (NSDictionary *)defaultTextAttributes;
- (void)redisplay;

- (void)sendWindowEvent: (NSString *)anEvent;
- (void)sendWindowEvent: (NSString *)anEvent withArg: (id)anArg;
-(void)sendWindowEventWithArgs: (NSString *)anEvent, ...;
@end

@interface World (Accessors)
- (NSWindow *)window;
- (NSString *)queuedRoomString;
- (void)setQueuedRoomString: (NSString *)aString;
- (MxWorldStatus)status;
- (void)setStatus: (MxWorldStatus)aStatus;
- (NSMutableString *)outputBuffer;
- (NSMutableString *)inputBuffer;
- (NSMutableArray *)inputHistory;
- (void)setInputHistory: (NSMutableArray *)aHistory;
- (MxWorldSettings *)settings;
- (void)setSettings: (MxWorldSettings *)someSettings;
- (NSTimer *)timer;
- (void)setTimer: (NSTimer *)aTimer;
@end