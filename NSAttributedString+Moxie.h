//
//  NSAttributedString+Moxie.h
//  Moxie
//
//  Created by Brian Cully on Tue Aug 24 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//
@interface NSAttributedString (Moxie)
+ (id)attributedStringWithString: (NSString *)aString;
+ (id)attributedStringWithString: (NSString *)aString
                      attributes: (NSDictionary *)attributes;
+ (NSAttributedString *)attributedStringWithForm: (NSArray *)aForm
                               defaultAttributes: (NSDictionary *)defaultAttributes;
@end