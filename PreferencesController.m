#import "PreferencesController.h"

@implementation PreferencesController
+ (PreferencesController *)sharedController
{
    static PreferencesController *sharedController = nil;
    
    if (sharedController == nil) {
        sharedController = [[PreferencesController alloc] init];
    }
    return sharedController;
}

- (id)init
{
    self = [self initWithWindowNibName: @"Preferences"];
    [self setWindowFrameAutosaveName: @"preferencesWindow"];
    return self;
}

- (IBAction)addStartupWorld: (id)sender
{
    NSOpenPanel *openPanel;
    
    openPanel = [NSOpenPanel openPanel];
    
    void (^openHandler)(NSInteger) = ^(NSInteger result)
    {
        if (result == NSFileHandlingPanelOKButton) {
            NSEnumerator *pathEnum;
            NSMutableArray *newContents;
            NSString *path;
            
            newContents = [NSMutableArray arrayWithArray: [[NSUserDefaults standardUserDefaults] startupWorlds]];
            pathEnum = [[openPanel URLs] objectEnumerator];
            while ((path = [pathEnum nextObject]) != nil) {
                [newContents addObject: path];
            }
            
            [[NSUserDefaults standardUserDefaults] setStartupWorlds: newContents];
            [self refreshStartupTableView];            
        }
    };
    
    [openPanel setAllowsMultipleSelection: YES];
    [openPanel beginSheetModalForWindow:[self window] completionHandler: openHandler];
}

- (IBAction)removeStartupWorld: (id)sender
{
	NSIndexSet *selectedIndexes;
    NSMutableArray *newStartupItems;
    __block int removedRows;
    
    newStartupItems = [[[NSUserDefaults standardUserDefaults] startupWorlds] mutableCopy];
    removedRows = 0;
    selectedIndexes = [theStartupItemsTableView selectedRowIndexes];
	[selectedIndexes enumerateIndexesUsingBlock: ^(NSUInteger idx, BOOL *stop) {
        [newStartupItems removeObjectAtIndex: idx-removedRows];
        removedRows++;
	}];
    [[NSUserDefaults standardUserDefaults] setStartupWorlds: newStartupItems];
    [self refreshStartupTableView];
}

- (void)refreshStartupTableView
{
    NSEnumerator *startupEnum;
    NSMutableArray *newContent;
    NSString *path;
    
    newContent = [NSMutableArray array];
    startupEnum = [[[NSUserDefaults standardUserDefaults] startupWorlds] objectEnumerator];
    while ((path = [startupEnum nextObject]) != nil) {
        NSDictionary *tmpDict;
        
        tmpDict = [NSDictionary dictionaryWithObject: [[path lastPathComponent] stringByDeletingPathExtension]
                                              forKey: @"name"];
        [newContent addObject: tmpDict];
    }
    [theStartupItemsController setContent: newContent];
}

- (void)windowDidLoad
{
    NSUserDefaults *prefs;
    [super windowDidLoad];

    // Set the colors in the status panel. NSPreferencesController doesn't use accessor methods, so
    // we have to do this by hand.
    prefs = [NSUserDefaults standardUserDefaults];
    [prefs setRecentActivityColorData: [prefs recentActivityColorData]];
    [prefs setConnectedColorData: [prefs connectedColorData]];
    [prefs setDisconnectedColorData: [prefs disconnectedColorData]];
    [self refreshStartupTableView];

    [theStartupItemsTableView registerForDraggedTypes:
        [NSArray arrayWithObjects: NSFilenamesPboardType, NSURLPboardType, nil]];
}

- (NSString *)REPLFontName
{
    return [[NSUserDefaults standardUserDefaults] REPLFontName];
}
@end

@implementation PreferencesController (StartupItemTableViewDataSource)
- (NSDragOperation)tableView: (NSTableView *)aTableView
                validateDrop: (id <NSDraggingInfo>)sender
                 proposedRow: (int)i
       proposedDropOperation: (NSDragOperation)operation;
{
    NSArray *paths;
    NSPasteboard *pb;
    
    pb = [sender draggingPasteboard];
    paths = [pb propertyListForType: NSFilenamesPboardType];
    if ([[paths objectAtIndex: 0] hasSuffix: @".moxie"]) {
        [aTableView setDropRow: -1 dropOperation: NSTableViewDropOn];
        return NSDragOperationLink;
    }
    return NSDragOperationNone;
}

- (BOOL)tableView: (NSTableView *)aTableView
       acceptDrop: (id <NSDraggingInfo>)sender
              row: (int)anIndex
    dropOperation: (NSTableViewDropOperation)anOperation
{
    NSEnumerator *pathEnum;
    NSMutableArray *newContents;
    NSPasteboard *pb;
    NSString *path;
    
    newContents = [NSMutableArray arrayWithArray: [[NSUserDefaults standardUserDefaults] startupWorlds]];
    pb = [sender draggingPasteboard];
    pathEnum = [[pb propertyListForType: NSFilenamesPboardType] objectEnumerator];
    while ((path = [pathEnum nextObject]) != nil) {
        if ([path hasSuffix: @".moxie"]) {
            [newContents addObject: path];
        }
    }
    
    [[NSUserDefaults standardUserDefaults] setStartupWorlds: newContents];
    [self refreshStartupTableView];
    return [newContents count] > 0;
}
@end