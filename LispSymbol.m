//
//  LispSymbol.m
//  Moxie
//
//  Created by Brian Cully on Tue Sep 07 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "LispSymbol.h"

@implementation LispSymbol
LispSymbol *BIG_T = nil;
LispSymbol *BIG_NIL = nil;

+ (LispSymbol *)symbolT
{
    if (BIG_T == nil)
        BIG_T = [[self alloc] initWithName: @"T"];
    return BIG_T;
}

+ (LispSymbol *)symbolNIL
{
    if (BIG_NIL == nil)
        BIG_NIL = [[self alloc] initWithName: @"NIL"];
    return BIG_NIL;
}

+ (LispSymbol *)symbolNamed: (NSString *)aName
{
    if ([aName caseInsensitiveCompare: @"T"] == NSOrderedSame)
        return [LispSymbol symbolT];
    else if ([aName caseInsensitiveCompare: @"NIL"] == NSOrderedSame)
        return [LispSymbol symbolNIL];
    else
        return [[[self alloc] initWithName: aName] autorelease];
}

- (id)initWithName: (NSString *)aName
{
    self = [super init];
    if (self) {
        [self setName: aName];
    }
    return self;
}

- (void)dealloc
{
    [self setName: nil];
    [super dealloc];
}

- (NSString *)description
{
    return [NSString stringWithFormat: @"#<LispSymbol: %@>", [self name]];
}

- (BOOL)boolValue
{
    return [self isEqualToString: @"NIL"] == NO;
}

- (double)doubleValue
{
    return [[self name] doubleValue];
}

- (float)floatValue
{
    return [[self name] floatValue];
}

- (int)intValue
{
    return [[self name] intValue];
}

- (NSString *)stringValue
{
    return [self name];
}

- (id)copyWithZone: (NSZone *)aZone
{
    LispSymbol *result;

    result = [[LispSymbol allocWithZone: aZone] init];
    [result setName: [self name]];
    return result;
}

- (unsigned)hash
{
    return [[self name] hash];
}

- (BOOL)isEqual: (id)anObject
{
    BOOL result;

    if ([anObject isKindOfClass: [NSString class]])
        result = [self isEqualToString: anObject];
    else if ([anObject isKindOfClass: [self class]]) {
        result = [self isEqualToString: [anObject name]];
    } else
        result = [super isEqual: anObject];
    return result;
}

- (NSString *)lispForm
{
    return [self name];
}
@end

@implementation LispSymbol (Accessors)
- (NSString *)name
{
    return theName;
}

- (void)setName: (NSString *)aName
{
    [aName retain];
    [theName release];
    theName = aName;
}
@end

@implementation LispSymbol (Comparators)
- (BOOL)isEqualToString: (NSString *)aString
{
    return ([[self name] caseInsensitiveCompare: aString] == NSOrderedSame);
}
@end