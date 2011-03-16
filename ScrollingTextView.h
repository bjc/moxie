/* ScrollingTextView */

#import <Cocoa/Cocoa.h>

@interface ScrollingTextView : NSTextView <NSLayoutManagerDelegate>
{
    IBOutlet NSResponder *theResponder;
}
@end