//
//  NSDictionary+LispExtensions.h
//  Moxie
//
//  Created by Brian Cully on Mon Sep 13 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSDictionary (LispExtensions)
+ (NSMutableDictionary *)dictionaryWithAlist: (NSArray *)attrList;

- (NSString *)lispForm;
@end