//
//  NSUserDefaults+Moxie.h
//  Moxie
//
//  Created by Brian Cully on Thu Jan 15 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSUserDefaults (MoxieDefaults)
- (BOOL)startupShowWorldSelector;
- (BOOL)selectorIsFloating;
- (BOOL)selectorIsAlwaysVisible;
- (NSArray *)startupWorlds;
- (void)setStartupWorlds: (NSArray *)someWorlds;
- (NSString *)logDirectory;

- (NSData *)connectedColorData;
- (void)setConnectedColorData: (NSData *)colorData;
- (NSData *)disconnectedColorData;
- (void)setDisconnectedColorData: (NSData *)colorData;
- (NSData *)recentActivityColorData;
- (void)setRecentActivityColorData: (NSData *)colorData;

- (NSData *)REPLInputTextColorData;
- (void)setREPLInputTextColorData: (NSData *)colorData;
- (NSData *)REPLOutputTextColorData;
- (void)setREPLOutputTextColorData: (NSData *)colorData;
- (NSData *)REPLPromptColorData;
- (void)setREPLPromptColorData: (NSData *)colorData;
- (NSData *)REPLReturnValueColorData;
- (void)setREPLReturnValueColorData: (NSData *)colorData;
- (NSData *)REPLBackgroundColorData;
- (void)setREPLBackgroundColorData: (NSData *)colorData;
- (NSData *)REPLFontData;
- (void)setREPLFontData: (NSData *)fontData;
@end


@interface NSUserDefaults (MoxieTranslators)
- (NSColor *)connectedColor;
- (NSColor *)disconnectedColor;
- (NSColor *)recentActivityColor;
- (NSColor *)REPLInputTextColor;
- (NSColor *)REPLOutputTextColor;
- (NSColor *)REPLPromptColor;
- (NSColor *)REPLReturnValueColor;
- (NSColor *)REPLBackgroundColor;
- (NSFont *)REPLFont;
- (NSString *)REPLFontName;
@end