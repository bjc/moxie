#import "WorldStatusController.h"

#import "World.h"

@interface World (TableDescription)
- (NSMutableDictionary *)worldDescription;
@end

@implementation World (TableDescription)
- (NSMutableDictionary *)worldDescription
{
    NSMutableDictionary *worldAttrs;
    NSString *statusString;
    
    switch ([self status]) {
        case MxConnected:
            statusString = @"c";
            break;
        case MxDisconnected:
            statusString = @"d";
            break;
        case MxRecentActivity:
            statusString = @"!";
            break;
        default:
            statusString = @"";
    }

    worldAttrs = [NSMutableDictionary dictionaryWithObjects:
        [NSArray arrayWithObjects: [self displayName], statusString, nil]
                                                    forKeys:
        [NSArray arrayWithObjects: @"connection", @"status", nil]];
    
    return worldAttrs;
}
@end

@implementation WorldStatusController (TableViewDelegate)
- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
    NSArray *worlds;
    int row;

    // Make the selected world the main window.
    worlds = [[NSDocumentController sharedDocumentController] documents];
    row = [theTableView selectedRow];
    if (row >= 0 && (unsigned)row < [worlds count]) {
        [[[worlds objectAtIndex: row] windowForSheet] makeKeyAndOrderFront: self];
        [NSApp activateIgnoringOtherApps: YES];
    }
}

- (void)tableView: (NSTableView *)aTableView
  willDisplayCell: (id)aCell
   forTableColumn: (NSTableColumn *)aTableColumn
              row: (int)rowIndex
{
    NSArray *worlds;
    
    worlds = [[NSDocumentController sharedDocumentController] documents];
    if (rowIndex >= 0 && (unsigned)rowIndex < [worlds count]) {
        NSColor *cellColor;
        NSFont *font;
        World *world;
        
        font = [[NSFontManager sharedFontManager] fontWithFamily: @"Helvetica"
                                                          traits: NSUnboldFontMask | NSUnitalicFontMask
                                                          weight: 9.0
                                                            size: 12.0];
        world = [worlds objectAtIndex: rowIndex];
        switch ([world status]) {
            case MxConnected:
                cellColor = [[NSUserDefaults standardUserDefaults] connectedColor];
                break;
            case MxDisconnected:
                cellColor = [[NSUserDefaults standardUserDefaults] disconnectedColor];
                font = [[NSFontManager sharedFontManager] convertFont: font toHaveTrait: NSItalicFontMask];
                break;
            case MxRecentActivity:
                cellColor = [[NSUserDefaults standardUserDefaults] recentActivityColor];
                font = [[NSFontManager sharedFontManager] convertFont: font toHaveTrait: NSBoldFontMask];
                break;
            default:
                cellColor = [NSColor blackColor];
                break;
        }
        [aCell setFont: font];
        [aCell setTextColor: cellColor];
    }
}
@end

@implementation WorldStatusController (TableViewDataSource)
- (int)numberOfRowsInTableView: (NSTableView *)aTableView
{
    return [[[NSDocumentController sharedDocumentController] documents] count];
}

- (id)tableView: (NSTableView *)aTableView
objectValueForTableColumn: (NSTableColumn *)aTableColumn
            row: (int)rowIndex
{
    NSArray *worlds;

    worlds = [[NSDocumentController sharedDocumentController] documents];
    if (rowIndex >= 0 && (unsigned)rowIndex < [worlds count])
        return [[[worlds objectAtIndex: rowIndex] worldDescription] objectForKey: [aTableColumn identifier]];
    return nil;
}
@end

@implementation WorldStatusController
+ (WorldStatusController *)sharedController
{
    static WorldStatusController *sharedController = nil;
    
    if (sharedController == nil) {
        sharedController = [[WorldStatusController alloc] init];
    }
    return sharedController;
}

- (id)init
{
    self = [self initWithWindowNibName: @"WorldSelector"];
    [self setWindowFrameAutosaveName: @"worldStatusWindow"];
    return self;
}

- (void)dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver: self];
    
    [super dealloc];
}

- (void)selectWorld: (World *)aWorld
{
    NSArray *worlds;
    int row;
    
    if (aWorld) {
        worlds = [[NSDocumentController sharedDocumentController] documents];
        row = [worlds indexOfObject: aWorld];
        [theTableView selectRow: row byExtendingSelection: NO];
    } else
        [theTableView deselectAll: self];
}

- (void)setFloating: (BOOL)isFloating
{
    [(NSPanel *)[self window] setFloatingPanel: isFloating];
}

- (void)setAlwaysVisible: (BOOL)isAlwaysVisible
{
    [(NSPanel *)[self window] setHidesOnDeactivate: isAlwaysVisible ? NO : YES];
}

- (void)windowDidLoad
{
    NSNotificationCenter *defaultCenter;
    
    [super windowDidLoad];

    defaultCenter = [NSNotificationCenter defaultCenter];
    [defaultCenter addObserver: self
                      selector: @selector(userDefaultsChanged:)
                          name: NSUserDefaultsDidChangeNotification
                        object: [NSUserDefaults standardUserDefaults]];
    [defaultCenter addObserver: self
                      selector: @selector(mainWindowChanged:)
                          name: NSWindowDidBecomeMainNotification
                        object: nil];
    [defaultCenter addObserver: self
                      selector: @selector(windowClosed:)
                          name: NSWindowWillCloseNotification
                        object: nil];

    [self setFloating: [[NSUserDefaults standardUserDefaults] selectorIsFloating]];
    [self setAlwaysVisible: [[NSUserDefaults standardUserDefaults] selectorIsAlwaysVisible]];
    [self selectWorld: [[NSDocumentController sharedDocumentController] currentDocument]];
}

- (void)update
{
    [theTableView reloadData];
}

- (void)userDefaultsChanged: (NSNotification *)aNotification
{
    [self setFloating: [[NSUserDefaults standardUserDefaults] selectorIsFloating]];
    [self setAlwaysVisible: [[NSUserDefaults standardUserDefaults] selectorIsAlwaysVisible]];
    [self update];
}

- (void)mainWindowChanged: (NSNotification *)aNotification
{
    World *world;
    
    world = [[[aNotification object] windowController] document];
    if ([world isKindOfClass: [World class]]) {
        if ([world status] == MxRecentActivity)
            [world setStatus: MxConnected];
        [self update];
        [self selectWorld: world];
        
        [world showWindows];
        [NSApp activateIgnoringOtherApps: YES];
    }
}

- (void)windowClosed: (NSNotification *)aNotification
{
    [theTableView reloadData];
    [self selectWorld: nil];
}
@end