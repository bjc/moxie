//
//  NSNumber+LispExtensions.m
//  Moxie
//
//  Created by Brian Cully on Tue Sep 28 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "NSNumber+LispExtensions.h"

@implementation NSNumber (LispExtensions)
- (NSString *)lispForm
{
    return [self description];
}
@end