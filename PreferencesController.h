/* PreferencesController */

@interface PreferencesController : NSWindowController
{
    IBOutlet NSArrayController *theStartupItemsController;
    IBOutlet NSTableView *theStartupItemsTableView;
}

+ (PreferencesController *)sharedController;

- (IBAction)addStartupWorld: (id)sender;
- (IBAction)removeStartupWorld: (id)sender;

- (void)refreshStartupTableView;
@end