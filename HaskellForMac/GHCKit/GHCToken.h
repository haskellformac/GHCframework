//
//  GHCToken.h
//  GHCKit
//
//  Created by Manuel M T Chakravarty on 26/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#ifndef HaskellForMac_GHCToken_h
#define HaskellForMac_GHCToken_h

#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, GHCToken) {
  GHCTokenAs,
  GHCTokenCase,
  GHCTokenClass,
  GHCTokenData,

  GHCTokenDotdot,
  GHCTokenColon,
  GHCTokenDcolon,
  GHCTokenEqual,

  GHCTokenVarsym,
  GHCTokenConsym,
  GHCTokenVarid,
  GHCTokenConid,
  GHCTokenQvarsym,
  GHCTokenQconsym,
  GHCTokenQvarid,
  GHCTokenQconid,

  GHCTokenInteger,
  GHCTokenRational,

  GHCTokenChar,
  GHCTokenString,
  GHCTokenLineComment,
  GHCTokenBlockComment,
  GHCTokenOther             // a token type we are not interested in
};


@interface GHCLocatedToken : NSObject

@property GHCToken   token;
@property NSUInteger line;
@property NSUInteger column;
@property NSUInteger lines;
@property NSUInteger endColumn;

- (instancetype)initWithToken:(GHCToken)token
                         line:(NSUInteger)line
                       column:(NSUInteger)column
                        lines:(NSUInteger)lines
                    endColumn:(NSUInteger)endColumn;

@end

#endif
