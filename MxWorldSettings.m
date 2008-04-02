//
//  MxWorldSettings.m
//  Moxie
//
//  Created by Brian Cully on Thu Jan 01 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "MxWorldSettings.h"

#include <Security/SecKeychain.h>
#include <Security/SecKeychainItem.h>
#include <Security/SecAccess.h>
#include <Security/SecTrustedApplication.h>
#include <Security/SecACL.h>

#define MxHostnameSettingName ":HOSTNAME"
#define MxPortSettingName ":PORT"

@implementation MxWorldSettings
- (NSMutableDictionary *)settings
{
    return theSettings;
}

- (void)setSettings: (NSMutableDictionary *)someSettings
{
    [someSettings retain];
    [theSettings release];
    theSettings = someSettings;
}

+ (MxWorldSettings *)settingsWithDictionary: (NSDictionary *)aDictionary
{
    MxWorldSettings *result;

    result = [[[MxWorldSettings alloc] init] autorelease];
    [result setSettings: [NSMutableDictionary dictionaryWithDictionary: aDictionary]];
    return result;
}

+ (MxWorldSettings *)settingsFromDefaults
{
    MxWorldSettings *tmpSettings;
    NSUserDefaults *defaults;
    
    tmpSettings = [[[MxWorldSettings alloc] init] autorelease];
    defaults = [NSUserDefaults standardUserDefaults];
    if ([defaults objectForKey: @":CONNECT-ON-OPEN"])
        [tmpSettings setConnectOnOpen: [[defaults objectForKey: @":CONNECT-ON-OPEN"] boolValue]];
    else
        [tmpSettings setConnectOnOpen: NO];
    
    if ([defaults objectForKey: @":TEXT-COLOR"])
        [tmpSettings setTextColor:
            [NSUnarchiver unarchiveObjectWithData: [defaults objectForKey: @":TEXT-COLOR"]]];
    else
        [tmpSettings setTextColor: [NSColor blackColor]];
    
    if ([defaults objectForKey: @":BACKGROUND-COLOR"])
        [tmpSettings setBackgroundColor:
            [NSUnarchiver unarchiveObjectWithData: [defaults objectForKey: @":BACKGROUND-COLOR"]]];
    else
        [tmpSettings setBackgroundColor: [NSColor whiteColor]];
    
    if ([defaults objectForKey: @":FONT"])
        [tmpSettings setFont:
            [NSUnarchiver unarchiveObjectWithData: [defaults objectForKey: @":FONT"]]];
    else
        [tmpSettings setFont: [NSFont fontWithName: @"Monaco" size: 10.0]];
    
    [tmpSettings setLoggingEnabled: [defaults boolForKey: @":LOGGING-ENABLED"]];
    [tmpSettings setLogTimeStamp: [defaults boolForKey: @":LOG-TIME-STAMPS"]];
    [tmpSettings setLogInput: [defaults boolForKey: @":LOG-INPUT"]];
    
    return tmpSettings;
}

- (id)init
{
    self = [super init];
    if (self) {
        [self setSettings: [NSMutableDictionary dictionary]];
    }
    return self;
}

- (void)dealloc
{
    [self setSettings: nil];
    
    [super dealloc];
}

- (NSString *)description
{
    return [[self settings] description];
}

- (id)objectForKey: (NSString *)aKey
{
    id value;

    value = [[self settings] objectForKey: [LispSymbol symbolNamed: aKey]];
    if ([value isKindOfClass: [NSArray class]] && [value count] == 1)
        return [value objectAtIndex: 0];
    return value;
}

- (void)removeObjectForKey: (NSString *)aKey
{
    id oldObject;
    
    oldObject = [[self settings] objectForKey: [LispSymbol symbolNamed: aKey]];
    if (oldObject) {
        NSDictionary *userInfo;
        NSNotificationCenter *defaultCenter;

        userInfo = [NSDictionary dictionaryWithObjectsAndKeys:
            aKey, MxSettingName, oldObject, MxSettingOldValue, nil];

        [[self settings] removeObjectForKey: aKey];
        
        defaultCenter = [NSNotificationCenter defaultCenter];
        [defaultCenter postNotificationName: MxWorldSettingsDidChangeNotification
                                     object: self
                                   userInfo: userInfo];
        
    }
}

- (void)setObject: (id)anObject forKey: (NSString *)aKey
{
    if (anObject) {
        NSDictionary *userInfo;
        NSNotificationCenter *defaultCenter;
        id oldObject;
        
        userInfo = nil;
        oldObject = [[self settings] objectForKey: [LispSymbol symbolNamed: aKey]];
        if (!(oldObject == anObject || [oldObject isEqual: anObject])) {
            if (oldObject)
                userInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                    aKey, MxSettingName, oldObject, MxSettingOldValue, nil];
            
            [[self settings] setObject: anObject forKey: [LispSymbol symbolNamed: aKey]];
            
            defaultCenter = [NSNotificationCenter defaultCenter];
            [defaultCenter postNotificationName: MxWorldSettingsDidChangeNotification
                                         object: self
                                       userInfo: userInfo];
        }
    } else
        [self removeObjectForKey: aKey];
}

- (BOOL)boolForKey: (NSString *)aKey
{
    id value;

    value = [self objectForKey: aKey];
    if ([value isKindOfClass: [NSArray class]]) {
        if ([value count] == 0)
            return NO;
        value = [value objectAtIndex: 0];
    }
    
    if ([value respondsToSelector: @selector(boolValue)]) {
        NSInvocation *invocation;
        NSMethodSignature *sig;

        sig = [value methodSignatureForSelector: @selector(boolValue)];
        if ([sig methodReturnLength] == sizeof(BOOL)) {
            BOOL rc;

            invocation = [NSInvocation invocationWithMethodSignature: sig];
            [invocation setTarget: value];
            [invocation setSelector: @selector(boolValue)];
            [invocation invoke];
            [invocation getReturnValue: &rc];
            
            return rc;
        }
    }
    return NO;
}

- (void)setBool: (BOOL)aValue forKey: (NSString *)aKey
{
    [self setObject: aValue ? [LispSymbol symbolT] : [LispSymbol symbolNIL]
             forKey: aKey];
}

- (NSString *)stringForKey: (NSString *)aKey
{
    NSString *value;
    
    value = [[[self settings] objectForKey: aKey] objectAtIndex: 0];
    if ([value respondsToSelector: @selector(stringValue)])
        return [value performSelector: @selector(stringValue)];
    else
        return value;
}
@end

@implementation MxWorldSettings (MoxieSettings)
- (NSString *)hostname
{
    return [self stringForKey: @":HOSTNAME"];
}

- (void)setHostname: (NSString *)aHostname
{
    [self setObject: aHostname forKey: @":HOSTNAME"];
}

- (NSNumber *)port
{
    return [NSNumber numberWithInt: [[self objectForKey: @":PORT"] intValue]];
}

- (void)setPort: (NSNumber *)aPort
{
    [self setObject: aPort forKey: @":PORT"];
}

- (BOOL)connectOnOpen
{
    return [self boolForKey: @":CONNECT-ON-OPEN"];
}

- (void)setConnectOnOpen: (BOOL)shouldConnect
{
    [self setBool: shouldConnect forKey: @":CONNECT-ON-OPEN"];
}

- (NSString *)character
{
    return [self stringForKey: @":CHARACTER"];
}

- (void)setCharacter: (NSString *)aName
{
    [self setObject: aName forKey: @":CHARACTER"];
}

- (NSString *)password
{
    return [self stringForKey: @":PASSWORD"];
}

- (void)setPassword: (NSString *)aPassword
{
    [self setObject: aPassword forKey: @":PASSWORD"];
}

/*
- (NSString *)password
{
    if ([[self characterName] length] > 0 &&
        [[self hostname] length] > 0 &&
        [self port]) {
        const char *serverName, *accountName;
        void *pwData;
        UInt32 pwLen;
        
        serverName = [[self hostname] UTF8String];
        accountName = [[self characterName] UTF8String];
        if (SecKeychainFindInternetPassword(NULL, strlen(serverName), serverName, 0, NULL,
                                            strlen(accountName), accountName, 0, NULL,
                                            [[self port] unsignedLongValue],
                                            kSecProtocolTypeTelnet, kSecAuthenticationTypeDefault,
                                            &pwLen, &pwData, NULL) == noErr) {
            NSString *password;
            
            // We found a password in the keychain.
            password = [NSString stringWithUTF8String: pwData];
            return password;
        }
    }

    return nil;
}

- (void)setPassword: (NSString *)aPassword
{
    if ([[self characterName] length] > 0 &&
        [[self hostname] length] > 0 &&
        [self port]) {
        const char *serverName, *accountName, *pwData;
        UInt32 pwLen;

        serverName = [[self hostname] UTF8String];
        accountName = [[self characterName] UTF8String];
        pwData = [aPassword UTF8String];
        pwLen = strlen(pwData);
        SecKeychainAddInternetPassword(NULL, strlen(serverName), serverName, 0, NULL,
                                       strlen(accountName), accountName, 0, NULL,
                                       [[self port] unsignedLongValue],
                                       kSecProtocolTypeTelnet, kSecAuthenticationTypeDefault,
                                       pwLen, pwData, NULL);
    }
}
*/

- (NSColor *)textColor
{
    NSArray *rgbaVals;

    rgbaVals = [self objectForKey: @":TEXT-COLOR"];
    if (rgbaVals) {
        return [NSColor colorWithCalibratedRed: [[rgbaVals objectAtIndex: 0] floatValue]
                                         green: [[rgbaVals objectAtIndex: 1] floatValue]
                                          blue: [[rgbaVals objectAtIndex: 2] floatValue]
                                         alpha: [[rgbaVals objectAtIndex: 3] floatValue]];
    }
    return nil;
}

- (void)setTextColor: (NSColor *)aColor
{
    if (aColor == nil)
        [self removeObjectForKey: @":TEXT-COLOR"];
    else {
        NSColor *rgbColor;
        float r, g, b, a;

        rgbColor = [aColor colorUsingColorSpaceName: NSCalibratedRGBColorSpace];
        [rgbColor getRed: &r green: &g blue: &b alpha: &a];
        [self setObject: [NSArray arrayWithObjects:
            [NSNumber numberWithFloat: r], [NSNumber numberWithFloat: g], [NSNumber numberWithFloat: b],
            [NSNumber numberWithFloat: a], nil]
                 forKey: @":TEXT-COLOR"];
    }
}

- (NSColor *)backgroundColor
{
    NSArray *rgbaVals;
    
    rgbaVals = [self objectForKey: @":BACKGROUND-COLOR"];
    if (rgbaVals) {
        return [NSColor colorWithCalibratedRed: [[rgbaVals objectAtIndex: 0] floatValue]
                                         green: [[rgbaVals objectAtIndex: 1] floatValue]
                                          blue: [[rgbaVals objectAtIndex: 2] floatValue]
                                         alpha: [[rgbaVals objectAtIndex: 3] floatValue]];
    }
    return nil;
}

- (void)setBackgroundColor: (NSColor *)aColor
{
    if (aColor == nil)
        [self removeObjectForKey: @":BACKGROUND-COLOR"];
    else {
        NSColor *rgbColor;
        float r, g, b, a;
        
        rgbColor = [aColor colorUsingColorSpaceName: NSCalibratedRGBColorSpace];
        [rgbColor getRed: &r green: &g blue: &b alpha: &a];
        [self setObject: [NSArray arrayWithObjects:
            [NSNumber numberWithFloat: r], [NSNumber numberWithFloat: g], [NSNumber numberWithFloat: b],
            [NSNumber numberWithFloat: a], nil]
                 forKey: @":BACKGROUND-COLOR"];
    }
}

- (NSData *)textColorData
{
    return [NSArchiver archivedDataWithRootObject: [self textColor]];
}

- (void)setTextColorData: (NSData *)colorData
{
    return [self setTextColor: [NSUnarchiver unarchiveObjectWithData: colorData]];
}

- (NSData *)backgroundColorData
{
    return [NSArchiver archivedDataWithRootObject: [self backgroundColor]];
}

- (void)setBackgroundColorData: (NSData *)colorData
{
    return [self setBackgroundColor: [NSUnarchiver unarchiveObjectWithData: colorData]];
}

- (NSString *)fontDisplayName
{
    return [[self font] displayName];
}

// XXX: defaults probably shouldn't be here.
- (NSFont *)font
{
    NSArray *fontInfo;

    fontInfo = [self objectForKey: @":FONT"];
    if (fontInfo && [fontInfo count] == 2) {
        NSFont *result;

        result = [NSFont fontWithName: [fontInfo objectAtIndex: 0]
                                 size: [[fontInfo objectAtIndex: 1] floatValue]];
        if (!result)
            result = [NSFont fontWithName: @"Monaco" size: 10.0];
        return result;
    }
    return nil;
}

- (void)setFont: (NSFont *)aFont
{
    if (aFont == nil)
        [self removeObjectForKey: @":FONT"];
    else {
        NSArray *value = [NSArray arrayWithObjects:
            [aFont fontName], [NSNumber numberWithFloat: [aFont pointSize]], nil];

        [self setObject: value forKey: @":FONT"];
    }
}

- (BOOL)loggingEnabled
{
    return [self boolForKey: @":LOGGING-ENABLED"];
}

- (void)setLoggingEnabled: (BOOL)isEnabled
{
    [self setBool: isEnabled forKey: @":LOGGING-ENABLED"];
}

- (BOOL)logTimeStamp
{
    return [self boolForKey: @":LOG-TIME-STAMP"];
}

- (void)setLogTimeStamp: (BOOL)isEnabled
{
    [self setBool: isEnabled forKey: @":LOG-TIME-STAMP"];
}

- (BOOL)logInput
{
    return [self boolForKey: @":LOG-INPUT"];
}

- (void)setLogInput: (BOOL)isEnabled
{
    [self setBool: isEnabled forKey: @":LOG-INPUT"];
}

- (NSRect)windowFrame
{
    NSArray *windowSize, *windowOrigin;

    windowSize = [self objectForKey: @":WINDOW-SIZE"];
    windowOrigin = [self objectForKey: @":WINDOW-ORIGIN"];
    if (!windowSize || ! windowOrigin ||
        [windowSize count] != 2 || [windowOrigin count] != 2)
        return NSMakeRect(-1, -1, -1, -1);

    return NSMakeRect([[windowOrigin objectAtIndex: 0] floatValue],
                      [[windowOrigin objectAtIndex: 1] floatValue],
                      [[windowSize objectAtIndex: 0] floatValue],
                      [[windowSize objectAtIndex: 1] floatValue]);
}

- (void)setWindowFrame: (NSRect)aFrame
{
    NSArray *origin = [NSArray arrayWithObjects:
        [LispSymbol symbolNamed: [NSString stringWithFormat: @"%f", aFrame.origin.x]],
        [LispSymbol symbolNamed: [NSString stringWithFormat: @"%f", aFrame.origin.y]], nil];
    NSArray *size = [NSArray arrayWithObjects:
        [LispSymbol symbolNamed: [NSString stringWithFormat: @"%f", aFrame.size.width]],
        [LispSymbol symbolNamed: [NSString stringWithFormat: @"%f", aFrame.size.height]], nil];

    [self setObject: origin forKey: @":WINDOW-ORIGIN"];
    [self setObject: size forKey: @":WINDOW-SIZE"];
}

- (unsigned int)inputViewSize
{
    return [[self objectForKey: @":INPUT-VIEW-SIZE"] intValue];
}

- (void)setInputViewSize: (unsigned int)aSize
{
    [self setObject: [LispSymbol symbolNamed: [NSString stringWithFormat: @"%u", aSize]]
             forKey: @":INPUT-VIEW-SIZE"];
}
@end

@implementation NSUserDefaults (MxWorldSettingsAdditions)
- (void)setDefaultsFromWorldSettings: (MxWorldSettings *)someSettings
{
    [self setObject: [someSettings objectForKey: @":CONNECT-ON-OPEN"] forKey: @":CONNECT-ON-OPEN"];
    [self setObject: [someSettings objectForKey: @":TEXT-COLOR"] forKey: @":TEXT-COLOR"];
    [self setObject: [someSettings objectForKey: @":BACKGROUND-COLOR"] forKey: @":BACKGROUND-COLOR"];
    [self setObject: [someSettings objectForKey: @":FONT"] forKey: @":FONT"];
    [self setObject: [someSettings objectForKey: @":LOGGING-ENABLED"] forKey: @":LOGGING-ENABLED"];
    [self setObject: [someSettings objectForKey: @":LOG-TIME-STAMP"] forKey: @":LOG-TIME-STAMP"];
    [self setObject: [someSettings objectForKey: @":LOG-INPUT"] forKey: @":LOG-INPUT"];
}
@end