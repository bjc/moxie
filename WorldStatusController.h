/* WorldStatusController */

@interface WorldStatusController : NSWindowController
{
    IBOutlet NSTableView *theTableView;
}
+ (WorldStatusController *)sharedController;

- (void)update;
@end