#import "ScrollingTextView.h"

@implementation ScrollingTextView
- (id)initWithFrame: (NSRect)aFrame
{
    self = [super initWithFrame: aFrame];
    [[self layoutManager] setDelegate: self];
    return self;
}

- (void)layoutManager: (NSLayoutManager *)aLayoutManager
didCompleteLayoutForTextContainer: (NSTextContainer *)aTextContainer
                atEnd:(BOOL)flag
{
    if (flag) {
        NSClipView *clipView;
        
        clipView = (NSClipView *)[self superview];
        if ([clipView isKindOfClass: [NSClipView class]]) {
            [clipView scrollToPoint:
                [clipView constrainScrollPoint: NSMakePoint(0.0, [self frame].size.height)]];
            [[clipView superview] reflectScrolledClipView: clipView];
        }
    }
}

- (BOOL)performKeyEquivalent: (NSEvent *)anEvent
{
    NSDocument *targetDoc;
    BOOL rc;
    
    rc = NO;
    targetDoc = [[[self window] windowController] document];
    if ([targetDoc respondsToSelector: @selector(handleEvent:from:)])
        rc = [[targetDoc performSelector: @selector(handleEvent:from:)
                             withObject: anEvent
                              withObject: self] boolValue] ? YES : [super performKeyEquivalent: anEvent];
    return rc;
}

- (void)keyDown: (NSEvent *)anEvent
{
    if ([[self delegate] respondsToSelector: @selector(handleEvent:from:)]) {
        if ([[[self delegate] performSelector: @selector(handleEvent:from:) withObject: anEvent
                                   withObject:self] boolValue] == NO)
            [super keyDown: anEvent];
    } else
        [super keyDown: anEvent];
}

- (void)keyDownToSuper: (NSEvent *)anEvent
{
    [super keyDown: anEvent];
}

- (void)changeFont: (id)sender
{
    if ([[self delegate] respondsToSelector: @selector(changeFont:)]) {
        [[self delegate] performSelector: @selector(changeFont:)
                              withObject: sender];
    }
}
@end
