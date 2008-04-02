//
//  NSFileHandle+LispExtensions.h
//  Moxie
//
//  Created by Brian Cully on Fri Sep 10 2004.
//  Copyright (c) 2004 Brian Cully. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSFileHandle (LispExtensions)
- (id)readLispForm;
@end