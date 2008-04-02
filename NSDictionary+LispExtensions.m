//
//  NSDictionary+LispExtensions.m
//  Moxie
//
//  Created by Brian Cully on Mon Sep 13 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "NSDictionary+LispExtensions.h"

@implementation NSDictionary (LispExtensions)
/*
 * Assoc Lists are arrays of the form ((KEY0 VALUES0) (KEY1 VALUES1) ... (KEYN VALUESN)).
 * Useful for lisp, not for Cocoa where we can use NSDictionary.
 */
+ (NSMutableDictionary *)dictionaryWithAlist: (NSArray *)attrList
{
    NSEnumerator *objEnum;
    NSMutableDictionary *result;
    NSArray *row;
    
    result = [NSMutableDictionary dictionary];
    objEnum = [attrList objectEnumerator];
    while ((row = [objEnum nextObject]) != nil) {
        NSArray *subarray;
        NSRange valueRange;
        id key;
        
        key = [row objectAtIndex: 0];
        valueRange = NSMakeRange(1, [row count] - 1);
        subarray = [row subarrayWithRange: valueRange];
        [result setObject: subarray forKey: key];
    }
    return result;
}

- (NSString *)lispForm
{
    NSEnumerator *keyEnum;
    NSMutableString *result;
    id key;
    
    result = [NSMutableString stringWithString: @"("];
    keyEnum = [self keyEnumerator];
    while ((key = [keyEnum nextObject]) != nil) {
        id value;
        
        value = [self objectForKey: key];
        [result appendFormat: @"(%@ . %@)", [key lispForm], [value lispForm]];
    }
    [result appendString: @")"];
    return result;
}
@end