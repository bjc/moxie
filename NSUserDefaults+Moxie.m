//
//  NSUserDefaults+Moxie.m
//  Moxie
//
//  Created by Brian Cully on Thu Jan 15 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

@implementation NSUserDefaults (MoxieDefaults)
- (BOOL)startupShowWorldSelector
{
    if ([self objectForKey: @"startup.showWorldSelector"])
        return [self boolForKey: @"startup.showWorldSelector"];
    else
        return NO;
}

- (BOOL)selectorIsFloating
{
    if ([self objectForKey: @"selector.alwaysOnTop"])
        return [self boolForKey: @"selector.alwaysOnTop"];
    else
        return NO;
}

- (BOOL)selectorIsAlwaysVisible
{
    if ([self objectForKey: @"selector.alwaysVisible"])
        return [self boolForKey: @"selector.alwaysVisible"];
    else
        return NO;
}

- (NSArray *)startupWorlds
{
    return [self arrayForKey: @"startup.worlds"];
}

- (void)setStartupWorlds: (NSArray *)someWorlds
{
    [self setObject: someWorlds forKey: @"startup.worlds"];
}

- (NSString *)logDirectory
{
    return [@"~/Documents/Moxie Transcripts/" stringByExpandingTildeInPath];
}

- (NSData *)connectedColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"selector.colorConnected"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject: [NSColor blackColor]];
    return tmpData;
}

- (void)setConnectedColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"selector.colorConnected"];
    else
        [self setObject: colorData forKey: @"selector.colorConnected"];
}

- (NSData *)disconnectedColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"selector.colorDisconnected"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject: [NSColor blackColor]];
    return tmpData;
}

- (void)setDisconnectedColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"selector.colorDisconnected"];
    else
        [self setObject: colorData forKey: @"selector.colorDisconnected"];
}

- (NSData *)recentActivityColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"selector.colorNewActivity"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject: [NSColor blackColor]];
    return tmpData;
}

- (void)setRecentActivityColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"selector.colorNewActivity"];
    else
        [self setObject: colorData forKey: @"selector.colorNewActivity"];    
}

- (NSData *)REPLInputTextColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"REPL.inputTextColor"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject:
            [NSColor blackColor]];
    return tmpData;
}

- (void)setREPLInputTextColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"REPL.inputTextColor"];
    else
        [self setObject: colorData forKey: @"REPL.inputTextColor"];    
}

- (NSData *)REPLOutputTextColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"REPL.outputTextColor"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject:
            [NSColor blackColor]];
    return tmpData;
}

- (void)setREPLOutputTextColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"REPL.outputTextColor"];
    else
        [self setObject: colorData forKey: @"REPL.outputTextColor"];    
}

- (NSData *)REPLPromptColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"REPL.promptColor"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject:
            [NSColor blackColor]];
    return tmpData;
}

- (void)setREPLPromptColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"REPL.promptColor"];
    else
        [self setObject: colorData forKey: @"REPL.promptColor"];    
}

- (NSData *)REPLReturnValueColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"REPL.resultColor"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject:
            [NSColor whiteColor]];
    return tmpData;
}

- (void)setREPLReturnValueColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"REPL.resultColor"];
    else
        [self setObject: colorData forKey: @"REPL.resultColor"];    
}

- (NSData *)REPLBackgroundColorData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"REPL.backgroundColor"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject:
            [NSColor whiteColor]];
    return tmpData;
}

- (void)setREPLBackgroundColorData: (NSData *)colorData
{
    if (colorData == nil)
        [self removeObjectForKey:  @"REPL.backgroundColor"];
    else
        [self setObject: colorData forKey: @"REPL.backgroundColor"];    
}

- (NSData *)REPLFontData
{
    NSData *tmpData;
    
    tmpData = [self dataForKey: @"REPL.font"];
    if (tmpData == nil)
        tmpData = [NSArchiver archivedDataWithRootObject:
            [NSFont userFixedPitchFontOfSize: 0]];
    return tmpData;
}

- (void)setREPLFontData: (NSData *)fontData;
{
    if (fontData == nil)
        [self removeObjectForKey:  @"REPL.font"];
    else
        [self setObject: fontData forKey: @"REPL.font"];    
}
@end

@implementation NSUserDefaults (MoxieTranslators)
- (NSColor *)recentActivityColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self recentActivityColorData]];
}

- (NSColor *)connectedColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self connectedColorData]];
}

- (NSColor *)disconnectedColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self disconnectedColorData]];
}

- (NSColor *)REPLInputTextColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self REPLInputTextColorData]];
}

- (NSColor *)REPLOutputTextColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self REPLOutputTextColorData]];
}

- (NSColor *)REPLPromptColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self REPLPromptColorData]];
}

- (NSColor *)REPLReturnValueColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self REPLReturnValueColorData]];
}

- (NSColor *)REPLBackgroundColor
{
    return [NSUnarchiver unarchiveObjectWithData: [self REPLBackgroundColorData]];
}

- (NSFont *)REPLFont
{
    return [NSUnarchiver unarchiveObjectWithData: [self REPLFontData]];
}

- (NSString *)REPLFontName
{
    return [NSString stringWithFormat: @"%@ %0.1f", [[self REPLFont] fontName],
                                      [[self REPLFont] pointSize]];
}
@end