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
    [openPanel setAllowsMultipleSelection: YES];
    [openPanel beginSheetForDirectory: nil
                                 file: nil
                                types: [NSArray arrayWithObject: @"moxie"]
                       modalForWindow: [self window]
                        modalDelegate: self
                       didEndSelector: @selector(openPanelDidEnd:returnCode:contextInfo:)
                          contextInfo: NULL];
}

- (void)openPanelDidEnd: (NSOpenPanel *)sheet
             returnCode: (int)returnCode
            contextInfo: (void *)contextInfo
{
    if (returnCode == NSOKButton) {
        NSEnumerator *pathEnum;
        NSMutableArray *newContents;
        NSString *path;
        
        newContents = [NSMutableArray arrayWithArray: [[NSUserDefaults standardUserDefaults] startupWorlds]];
        pathEnum = [[sheet filenames] objectEnumerator];
        while ((path = [pathEnum nextObject]) != nil) {
            [newContents addObject: path];
        }
        
        [[NSUserDefaults standardUserDefaults] setStartupWorlds: newContents];
        [self refreshStartupTableView];
    }
}

- (IBAction)removeStartupWorld: (id)sender
{
    NSEnumerator *rowEnum;
    NSMutableArray *newStartupItems;
    NSNumber *row;
    int removedRows;
    
    newStartupItems = [[[NSUserDefaults standardUserDefaults] startupWorlds] mutableCopy];
    removedRows = 0;
    rowEnum = [theStartupItemsTableView selectedRowEnumerator];
    while ((row = [rowEnum nextObject]) != nil) {
        [newStartupItems removeObjectAtIndex: [row intValue]-removedRows];
        removedRows++;
    }
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