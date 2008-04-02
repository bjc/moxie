//
//  NSArray+LispExtensions.m
//  Moxie
//
//  Created by Brian Cully on Tue Sep 07 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "NSArray+LispExtensions.h"

@implementation NSArray (LispExtensions)
- (NSString *)lispForm
{
    NSEnumerator *objectEnum;
    NSMutableString *result;
    id obj;

    if ([self count] == 0)
        return @"NIL";
    
    objectEnum = [self objectEnumerator];
    result = [NSMutableString stringWithString: @"("];
    obj = [objectEnum nextObject];
    while (obj != nil) {
        NSString *form;
        
        form = [obj lispForm];
        [result appendString: form];
        obj = [objectEnum nextObject];
        if (obj)
            [result appendString: @" "];
    }
    [result appendString: @")"];
    return result;
}
@end