//
//  WorldSettingsController.h
//  Moxie
//
//  Created by Brian Cully on Thu Jan 01 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "MxWorldSettings.h"

@interface WorldSettingsController : NSWindowController
{
    IBOutlet NSObjectController *theSettingsController;
    IBOutlet NSTabView *theTabView;
    IBOutlet NSTabViewItem *theHostSettingsTab;
}
+ (WorldSettingsController *)sharedController;

- (IBAction)saveSettingsAsDefault: (id)sender;
- (IBAction)openLogDirectory: (id)sender;

- (void)showHostSettingsTab;
@end

@interface WorldSettingsController (Accessors)
- (MxWorldSettings *)settings;
- (void)setSettings: (MxWorldSettings *)someSettings;
@end