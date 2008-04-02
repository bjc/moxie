/* ScrollingTextView */

#import <Cocoa/Cocoa.h>

@interface ScrollingTextView : NSTextView
{
    IBOutlet NSResponder *theResponder;
}

- (void)keyDownToSuper: (NSEvent *)anEvent;
@end