//
//  WorldSettingsController.m
//  Moxie
//
//  Created by Brian Cully on Thu Jan 01 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "WorldSettingsController.h"

#import "World.h"

@implementation WorldSettingsController
+ (WorldSettingsController *)sharedController
{
    static WorldSettingsController *sharedController = nil;
    
    if (sharedController == nil) {
        sharedController = [[WorldSettingsController alloc] init];
    }
    return sharedController;
}

- (id)init
{
    self = [self initWithWindowNibName: @"WorldSettings"];
    [self setWindowFrameAutosaveName: @"worldSettingsWindow"];
    return self;
}

- (void)dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver: self];
    [super dealloc];
}

- (void)setMainWindow: (NSWindow *)aWindow
{
    World *world;
    
    world = [[aWindow windowController] document];
    if ([world isKindOfClass: [World class]]) {
        // XXX: this needs to be pluginified
        [self setSettings: [world settings]];
    }
}

- (void)windowDidLoad
{
    NSNotificationCenter *defaultCenter;
    
    [super windowDidLoad];
    
    [self setMainWindow: [NSApp mainWindow]];
    
    defaultCenter = [NSNotificationCenter defaultCenter];
    [defaultCenter addObserver: self
                      selector: @selector(mainWindowChanged:)
                          name: NSWindowDidBecomeMainNotification
                        object: nil];
    [defaultCenter addObserver: self
                      selector: @selector(mainWindowResigned:)
                          name: NSWindowDidResignMainNotification
                        object: nil];
}

- (void)mainWindowChanged: (NSNotification *)aNotification
{
    [self setMainWindow: [aNotification object]];
}

- (void)mainWindowResigned: (NSNotification *)aNotification
{
    [self setMainWindow: nil];
}

- (void)changeFont: (id)sender
{
    NSFont *oldFont, *newFont;
    
    oldFont = [[self settings] font];
    newFont = [sender convertFont: oldFont];
    [[self settings] setFont: newFont];
}

- (IBAction)saveSettingsAsDefault: (id)sender
{
    [[NSUserDefaults standardUserDefaults] setDefaultsFromWorldSettings: [self settings]];
}

- (IBAction)openLogDirectory: (id)sender
{
    [[NSWorkspace sharedWorkspace] openFile: [[NSUserDefaults standardUserDefaults] logDirectory]];
}

- (void)showHostSettingsTab
{
    [self showWindow: self];
    [theTabView selectTabViewItem: theHostSettingsTab];
}
@end

@implementation WorldSettingsController (Accessors)
- (MxWorldSettings *)settings
{
    return [theSettingsController content];
}

- (void)setSettings: (MxWorldSettings *)someSettings
{
    [theSettingsController setContent: someSettings];
}
@end