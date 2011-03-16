//
//  NSAttributedString+Moxie.m
//  Moxie
//
//  Created by Brian Cully on Tue Aug 24 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "NSAttributedString+Moxie.h"

@implementation NSAttributedString (Moxie)
+ (id)attributedStringWithString: (NSString *)aString
{
    NSAttributedString *retval;
    
    retval = [[self alloc] initWithString: aString];
    return [retval autorelease];
}

+ (id)attributedStringWithString: (NSString *)aString
                      attributes: (NSDictionary *)attributes
{
    NSAttributedString *retval;
    
    retval = [[self alloc] initWithString: aString
                               attributes: attributes];
    return [retval autorelease];
}

+ (NSAttributedString *)attributedStringWithForm: (NSArray *)aForm
                               defaultAttributes: (NSDictionary *)defaultAttributes
{
    NSArray *rangeForms, *attrRange;
    NSEnumerator *rangeEnum;
    NSMutableAttributedString *result;
    NSString *string;

    if ([aForm count] < 2)
        return nil;
    
    string = [aForm objectAtIndex: 0];
    rangeForms = [aForm objectAtIndex: 1];

    result = [NSMutableAttributedString attributedStringWithString: string attributes: defaultAttributes];
    rangeEnum = [rangeForms objectEnumerator];
    while ((attrRange = [rangeEnum nextObject]) != nil) {
        NSArray *tmpForm;
        NSDictionary *attrDict;
        NSMutableDictionary *myAttributes;
        NSRange range;
        
        attrDict = [NSDictionary dictionaryWithAlist: attrRange];
        myAttributes = [NSMutableDictionary dictionary];

        // Should handle :BOLD as well.
        tmpForm = [attrDict objectForKey: @":RANGE"];
        if (tmpForm)
            range = NSMakeRange([[tmpForm objectAtIndex: 0] intValue], [[tmpForm objectAtIndex: 1] intValue]);
        else
            range = NSMakeRange(0, [string length]);

        tmpForm = [attrDict objectForKey: @":FONT"];
        if (tmpForm) {
            NSFont *font;
            
            font = [NSFont fontWithName: [tmpForm objectAtIndex: 0]
                                   size: [[tmpForm objectAtIndex: 1] doubleValue]];
            if (font)
                [myAttributes setObject: font forKey: NSFontAttributeName];
            else
                NSLog(@"WARNING: Couldn't find font named: %@.", [tmpForm objectAtIndex: 0]);
        }
        
        tmpForm = [attrDict objectForKey: @":LINK"];
        if (tmpForm) {
            [myAttributes setObject: [NSURL URLWithString: [tmpForm objectAtIndex: 0]]
                             forKey: NSLinkAttributeName];
            [myAttributes setObject: [NSCursor pointingHandCursor]
                             forKey: NSCursorAttributeName];
        }

        tmpForm = [attrDict objectForKey: @":ITALIC"];
        if (tmpForm) {
            [myAttributes setObject: [NSNumber numberWithFloat: [[tmpForm objectAtIndex: 0] doubleValue]]
                             forKey: NSObliquenessAttributeName];
        }
        
        tmpForm = [attrDict objectForKey: @":SUPER"];
        if (tmpForm) {
            [myAttributes setObject: [NSNumber numberWithInt: [[tmpForm objectAtIndex: 0] intValue]]
                             forKey: NSSuperscriptAttributeName];
        }
        
        tmpForm = [attrDict objectForKey: @":UNDERLINE"];
        if (tmpForm) {
            [myAttributes setObject: [NSNumber numberWithInt: [[tmpForm objectAtIndex: 0] intValue]]
                             forKey: NSUnderlineStyleAttributeName];
        }

        tmpForm = [attrDict objectForKey: @":STRIKETHROUGH"];
        if (tmpForm) {
            [myAttributes setObject: [NSNumber numberWithInt: [[tmpForm objectAtIndex: 0] intValue]]
                             forKey: NSStrikethroughStyleAttributeName];
        }
        
        tmpForm = [attrDict objectForKey: @":COLOR"];
        if (tmpForm) {
            int r, g, b;
            
            r = [[tmpForm objectAtIndex: 0] intValue];
            g = [[tmpForm objectAtIndex: 1] intValue];
            b = [[tmpForm objectAtIndex: 2] intValue];
            [myAttributes setObject: [NSColor colorWithCalibratedRed: (r * 1.0) / 255
                                                               green: (g * 1.0) / 255
                                                                blue: (b * 1.0) / 255
                                                               alpha: 1.0]
                             forKey: NSForegroundColorAttributeName];
        }

        tmpForm = [attrDict objectForKey: @":BACKGROUND-COLOR"];
        if (tmpForm) {
            NSColor *bgColor;
            int r, g, b;
            float a;
            
            r = [[tmpForm objectAtIndex: 0] intValue];
            g = [[tmpForm objectAtIndex: 1] intValue];
            b = [[tmpForm objectAtIndex: 2] intValue];
            a = 1.0;
            bgColor = [defaultAttributes objectForKey: NSBackgroundColorAttributeName];
            if (bgColor) {
                a = [bgColor alphaComponent];
                NSLog(@"DEBUG:\t alpha: %f", a);
            }
            [myAttributes setObject: [NSColor colorWithCalibratedRed: (r * 1.0) / 255
                                                               green: (g * 1.0) / 255
                                                                blue: (b * 1.0) / 255
                                                               alpha: a]
                             forKey: NSBackgroundColorAttributeName];
        }
        

        // These two attributes are broken, because they rely on information we might
        // not have. To whit, the FG and BG colors of the open world.
        tmpForm = [attrDict objectForKey: @":BOLD"];
        if (tmpForm) {
            NSColor *preColor;
            CGFloat r, g, b, a;
            
            preColor = [myAttributes objectForKey: NSForegroundColorAttributeName];
            if (preColor == nil)
                preColor = [defaultAttributes objectForKey: NSForegroundColorAttributeName];
            if (preColor) {
                [preColor getRed: &r green: &g blue: &b alpha: &a];
                r += 0.3; g += 0.3; b += 0.3;
                r = MIN(r, 1.0);
                g = MIN(g, 1.0);
                b = MIN(b, 1.0);
                [myAttributes setObject: [NSColor colorWithCalibratedRed: r green: g blue: b alpha: a]
                                 forKey: NSForegroundColorAttributeName];
            }
        }
        
        tmpForm = [attrDict objectForKey: @":INVERSE"];
        if (tmpForm) {
            NSColor *fgColor, *bgColor;
            
            fgColor = [myAttributes objectForKey: NSForegroundColorAttributeName];
            if (fgColor == nil)
                fgColor = [defaultAttributes objectForKey: NSForegroundColorAttributeName];
            bgColor = [myAttributes objectForKey: NSBackgroundColorAttributeName];
            if (bgColor == nil)
                bgColor = [defaultAttributes objectForKey: NSBackgroundColorAttributeName];
            
            [fgColor retain];
            [bgColor retain];
            if (fgColor && bgColor) {
                [myAttributes setObject: bgColor
                                 forKey: NSForegroundColorAttributeName];
                [myAttributes setObject: fgColor
                                 forKey: NSBackgroundColorAttributeName];
            }
            [fgColor release];
            [bgColor release];
        }
        
        [result addAttributes: myAttributes range: range];
    }
    
    return result;
}
@end