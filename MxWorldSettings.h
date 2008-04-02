//
//  MxWorldSettings.h
//  Moxie
//
//  Created by Brian Cully on Thu Jan 01 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#define MxWorldSettingsDidChangeNotification @"MxWorldSettingsDidChangeNotification"
#define MxSettingName @"MxSettingName"
#define MxSettingOldValue @"MxSettingOldValue"

@interface MxWorldSettings : NSObject
{
    NSMutableDictionary *theSettings;
}
+ (MxWorldSettings *)settingsWithDictionary: (NSDictionary *)aDictionary;
+ (MxWorldSettings *)settingsFromDefaults;
- (NSString *)description;

- (id)objectForKey: (NSString *)aKey;
- (void)setObject: (id)anObject forKey: (NSString *)aKey;
- (void)removeObjectForKey: (NSString *)aKey;

- (BOOL)boolForKey: (NSString *)aKey;
- (void)setBool: (BOOL)aValue forKey: (NSString *)aKey;
- (NSString *)stringForKey: (NSString *)aKey;
@end

@interface MxWorldSettings (MoxieSettings)
- (NSString *)hostname;
- (void)setHostname: (NSString *)aHostname;
- (NSNumber *)port;
- (void)setPort: (NSNumber *)aPort;
- (BOOL)connectOnOpen;
- (void)setConnectOnOpen: (BOOL)shouldConnect;
- (NSString *)character;
- (void)setCharacter: (NSString *)aName;
- (NSString *)password;
- (void)setPassword: (NSString *)aPassword;
- (NSColor *)textColor;
- (void)setTextColor: (NSColor *)aColor;
- (NSColor *)backgroundColor;
- (void)setBackgroundColor: (NSColor *)aColor;
- (NSFont *)font;
- (void)setFont: (NSFont *)aFont;
- (BOOL)loggingEnabled;
- (void)setLoggingEnabled: (BOOL)isEnabled;
- (BOOL)logTimeStamp;
- (void)setLogTimeStamp: (BOOL)isEnabled;
- (BOOL)logInput;
- (void)setLogInput: (BOOL)isEnabled;

- (NSRect)windowFrame;
- (void)setWindowFrame: (NSRect)aFrame;
- (unsigned int)inputViewSize;
- (void)setInputViewSize: (unsigned int)aSize;
@end

@interface NSUserDefaults (MxWorldSettingsAdditions)
- (void)setDefaultsFromWorldSettings: (MxWorldSettings *)someSettings;
@end