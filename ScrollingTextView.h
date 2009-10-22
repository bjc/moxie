/* ScrollingTextView */

#import <Cocoa/Cocoa.h>

@interface ScrollingTextView : NSTextView <NSLayoutManagerDelegate>
{
    IBOutlet NSResponder *theResponder;
}

- (void)keyDownToSuper: (NSEvent *)anEvent;
@end