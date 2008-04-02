//
//  NSException+LiDebugging.m
//  Liaison
//
//  Created by Brian Cully on Sun Sep 14 2003.
//  Copyright (c) 2003 Brian Cully. All rights reserved.
//

@interface LiException : NSException
@end

@implementation LiException
+ (void)load
{
    [self poseAsClass: [NSException class]];
}

- (void)raise
{
    [super raise];
}
@end
