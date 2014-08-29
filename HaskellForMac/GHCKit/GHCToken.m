//
//  GHCToken.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 28/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "GHCToken.h"


@implementation GHCLocatedToken

- (instancetype)initWithToken:(GHCToken)token
                         line:(NSUInteger)line
                       column:(NSUInteger)column
                        lines:(NSUInteger)lines
                    endColumn:(NSUInteger)endColumn
{
  self = [super init];
  if (self) {
    _token     = token;
    _line      = line;
    _column    = column;
    _lines     = lines;
    _endColumn = endColumn;
  }
  return self;
}

@end