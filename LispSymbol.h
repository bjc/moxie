//
//  LispSymbol.h
//  Moxie
//
//  Created by Brian Cully on Tue Sep 07 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface LispSymbol : NSObject <NSCopying>
{
    NSString *theName;
}
+ (LispSymbol *)symbolT;
+ (LispSymbol *)symbolNIL;
+ (LispSymbol *)symbolNamed: (NSString *)aName;

- (id)initWithName: (NSString *)aName;
- (NSString *)lispForm;
@end

@interface LispSymbol (Accessors)
- (NSString *)name;
- (void)setName: (NSString *)aName;
@end

@interface LispSymbol (Comparators)
- (BOOL)isEqualToString: (NSString *)aString;
@end