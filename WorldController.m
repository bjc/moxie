#import "WorldController.h"
#import "World.h"

#import "LispREPLController.h"
#import "PreferencesController.h"
#import "WorldSettingsController.h"
#import "WorldStatusController.h"

#import <objc/objc-runtime.h>

@implementation WorldController

- (IBAction)showPreferencesWindow:(id)sender
{
    [[PreferencesController sharedController] showWindow: sender];
}

- (IBAction)showWorldSettingsWindow:(id)sender
{
    [[WorldSettingsController sharedController] showWindow: sender];
}

- (IBAction)showWorldStatusWindow:(id)sender
{
    [[WorldStatusController sharedController] showWindow: sender];
}

- (IBAction)showLispREPLWindow:(id)sender
{
    [[LispREPLController sharedController] showWindow: sender];
}

- (IBAction)cycleDocumentWindows: (id)sender
{
    NSArray *openDocuments;
    NSDocument *mainDocument;
    unsigned i, documentCount;
    
    mainDocument = [self currentDocument];
    openDocuments = [self documents];
    documentCount = [openDocuments count];
    for (i = 0; i < documentCount; i++) {
        if ([openDocuments objectAtIndex: (i % documentCount)] == mainDocument) {
            [[openDocuments objectAtIndex: ((i+1) % documentCount)] showWindows];
            break;
        }
    }
}

#if XXX
- (void)awakeFromNib
{
    NSEnumerator *startupEnum;
    NSString *path;
    
    startupEnum = [[[NSUserDefaults standardUserDefaults] startupWorlds] objectEnumerator];
    while ((path = [startupEnum nextObject]) != nil) {
        [self openDocumentWithContentsOfFile: path display: YES];
    }
}
#endif

- (void)applicationDidFinishLaunching: (NSNotification *)aNotification
{
    // Start up the embedded lisp.
    [LispREPLController sharedController];
//    if ([[LispREPL sharedREPL] isLoaded] == NO) {
//        NSNotificationCenter *defaultCenter;
//
//        defaultCenter = [NSNotificationCenter defaultCenter];
//        [defaultCenter addObserver: self
//                          selector: @selector(lispFinishedLoading:)
//                              name: LispFinishedLoadingNotification
//                            object: nil];
//    }

    [NSColor setIgnoresAlpha: NO];
    if ([[NSUserDefaults standardUserDefaults] startupShowWorldSelector]) {
        [self showWorldStatusWindow: self];
    }
}

- (void)reviewUnsavedDocumentsWithAlertTitle: (NSString *)title
                                 cancellable: (BOOL)cancellable
                                    delegate: (id)delegate
                        didReviewAllSelector: (SEL)didReviewAllSelector
                                 contextInfo: (void *)contextInfo
{
    NSDocument *document;
    NSEnumerator *docEnum;
    unsigned count;
    
    count = 0;
    docEnum = [[self documents] objectEnumerator];
    while ((document = [docEnum nextObject]) != nil) {
        if ([document isDocumentEdited])
            count++;
    }
    
    if (count > 1) {
        NSString *alertString, *infoString;
        int rc;
        
        alertString = [NSString stringWithFormat:
            @"You have %d open connections. Would you like to review them before quitting?", count];
        infoString = [NSString stringWithFormat:
            @"If you don't review your windows, any connections will be closed."];
        rc = NSRunAlertPanel(alertString, infoString, @"Review Worlds...", @"Close Connections", @"Cancel");
        switch (rc) {
            case NSAlertDefaultReturn:
                // Review changes.
                [self closeAllDocumentsWithDelegate: delegate
                                didCloseAllSelector: didReviewAllSelector
                                        contextInfo: contextInfo];
                break;
            case NSAlertAlternateReturn:
                // Close connections.
                objc_msgSend(delegate, didReviewAllSelector, self, YES, contextInfo);
                break;
            case NSAlertOtherReturn:
                // Cancel.
                objc_msgSend(delegate, didReviewAllSelector, self, NO, contextInfo);
                break;
        }
    } else if (count == 1) {
        // Bring up the one sheet, no matter what.
        [[[self documents] objectAtIndex: 0] canCloseDocumentWithDelegate: delegate
                                                      shouldCloseSelector: didReviewAllSelector
                                                              contextInfo: contextInfo];
    } else
        objc_msgSend(delegate, didReviewAllSelector, self, YES, contextInfo);        
}

- (BOOL)validateMenuItem: (NSMenuItem *)anItem
{
    if ([anItem action] == @selector(cycleDocumentWindows:)) {
        return [[self documents] count] > 1;
    }
    return YES;
}
@end