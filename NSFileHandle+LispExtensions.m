//
//  NSFileHandle+LispExtensions.m
//  Moxie
//
//  Created by Brian Cully on Fri Sep 10 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import "NSFileHandle+LispExtensions.h"

@implementation NSFileHandle (LispExtensions)
- (void)alertDidEndSelector: (NSAlert *)alert
                 returnCode: (int)returnCode
                contextInfo: (void *)contextInfo
{
    [NSApp terminate: self];
}

- (unichar)readNextCharFromBufferAndMove: (BOOL)moveFilePointer
{
    // XXX: SO not thread safe.
    static NSString *buffer = nil;
    static unsigned int bindex = 0;
    unichar c;

    if (buffer == nil || bindex >= [buffer length]) {
        NSData *formData;

        formData = [self availableData];
        if ([formData length] == 0) {
            [[NSException exceptionWithName: @"REPLDeath" reason: @"The plug in subsystem died." userInfo: nil] raise];
        } else {
            if (buffer)
                [buffer release];
            buffer = [[NSString alloc] initWithCString: [formData bytes]
                                                length: [formData length]];
            bindex = 0;
        }
    }
    c = [buffer characterAtIndex: bindex];
    if (moveFilePointer)
        bindex++;
    return c;
}

- (NSString *)readLispString
{
    NSMutableString *result;
    unichar c;
    
    result = [NSMutableString string];
    c = [self readNextCharFromBufferAndMove: YES];
    while ((c = [self readNextCharFromBufferAndMove: YES]) != '"') {
        if (c == '\\')
            c = [self readNextCharFromBufferAndMove: YES];
        [result appendFormat: @"%c", c];
    }
    return result;
}

- (LispSymbol *)readLispSymbol
{
    NSMutableString *formString;
    unichar c;

    formString = [NSMutableString string];
    c = [self readNextCharFromBufferAndMove: NO];
    while (c != '(' && c != ')' &&
           [[NSCharacterSet whitespaceAndNewlineCharacterSet] characterIsMember: c] == NO) {
        // Handle string lengths.
        if (c == '"') {
            [formString appendFormat: @"\"%@\"", [self readLispString]];
        } else {
            c = [self readNextCharFromBufferAndMove: YES];
            [formString appendFormat: @"%c", c];
        }
        
        // Get the next char without swallowing, so we can see if
        // we use it before we swallow.
        c = [self readNextCharFromBufferAndMove: NO];
    }
    return [LispSymbol symbolNamed: formString];
}

- (id)readLispAtom
{
    id result;
    unichar c;

    c = [self readNextCharFromBufferAndMove: NO];
    if (c == '"') {
        // Parse as a string.
        result = [self readLispString];
    } else {
        result = [self readLispSymbol];
    }
    return result;
}

- (id)readLispForm
{
    id result;
    unichar c;

    result = nil;
    
    // Advance past white space and unmatched parens.
    c = [self readNextCharFromBufferAndMove: NO];
    while ([[NSCharacterSet whitespaceAndNewlineCharacterSet] characterIsMember: c] ||
           c == ')') {
        [self readNextCharFromBufferAndMove: YES];
        c = [self readNextCharFromBufferAndMove: NO];
    }
    
    // Begin form parsing.
    if (c == '(') {
        NSMutableArray *form;
        
        // Recursively parse form until closing paren.
        [self readNextCharFromBufferAndMove: YES];
        form = [NSMutableArray array];
        while ([self readNextCharFromBufferAndMove: NO] != ')') {
            id obj;
            
            obj = [self readLispForm];
            if ([obj isEqual: @"."] == NO) {
                if ([obj isEqual: @"NIL"])
                    [form addObject: [NSArray array]];
                else
                    [form addObject: obj];
            }
        }
        [self readNextCharFromBufferAndMove: YES];
        result = form;
    } else
        result = [self readLispAtom];
    return result;
}
@end