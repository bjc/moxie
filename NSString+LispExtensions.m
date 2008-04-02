//
//  NSString+LispExtensions.m
//  Moxie
//
//  Created by Brian Cully on Sat Sep 04 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "NSString+LispExtensions.h"

@implementation NSString (LispExtensions)
- (NSString *)lispForm
{
    NSMutableString *result;
    NSRange subrange;
    unsigned int len, i;
    
    result = [NSMutableString stringWithString: @"\""];
    len = [self length];
    subrange.location = 0;
    for (i = 0; i < len; i++) {
        switch ([self characterAtIndex: i]) {
            case '\\':
            case '\"':
                subrange.length = i - subrange.location;
                [result appendString: [self substringWithRange: subrange]];
                [result appendString: @"\\"];
                subrange.location = i;
                break;
        }
    }
    if (subrange.location < len) {
        subrange.length = len - subrange.location;
        [result appendString: [self substringWithRange: subrange]];
    }
    [result appendString: @"\""];
                
    return result;
}
@end