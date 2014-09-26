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
  GHCTokenDefault,
  GHCTokenDeriving,
  GHCTokenDo,
  GHCTokenElse,
  GHCTokenHiding,
  GHCTokenIf,
  GHCTokenImport,
  GHCTokenIn,
  GHCTokenInfix,
  GHCTokenInfixl,
  GHCTokenInfixr,
  GHCTokenInstance,
  GHCTokenLet,
  GHCTokenModule,
  GHCTokenNewtype,
  GHCTokenOf,
  GHCTokenQualified,
  GHCTokenThen,
  GHCTokenType,
  GHCTokenWhere,
  GHCTokenForall,
  GHCTokenForeign,
  GHCTokenExport,
  GHCTokenLabel,
  GHCTokenDynamic,
  GHCTokenSafe,
  GHCTokenInterruptible,
  GHCTokenUnsafe,
  GHCTokenStdcallconv,
  GHCTokenCcallconv,
  GHCTokenCapiconv,
  GHCTokenPrimcallconv,
  GHCTokenJavascriptcallconv,
  GHCTokenMdo,
  GHCTokenFamily,
  GHCTokenRole,
  GHCTokenGroup,
  GHCTokenBy,
  GHCTokenUsing,
  GHCTokenPattern,
  GHCTokenCtype,

  GHCTokenDotdot,
  GHCTokenColon,
  GHCTokenDcolon,
  GHCTokenEqual,
  GHCTokenLam,
  GHCTokenLcase,
  GHCTokenVbar,
  GHCTokenLarrow,
  GHCTokenRarrow,
  GHCTokenAt,
  GHCTokenTilde,
  GHCTokenTildehsh,
  GHCTokenDarrow,
  GHCTokenMinus,
  GHCTokenBang,
  GHCTokenStar,
  GHCTokenDot,
  GHCTokenBiglam,
  GHCTokenOcurly,
  GHCTokenCcurly,
  GHCTokenVocurly,
  GHCTokenVccurly,
  GHCTokenObrack,
  GHCTokenOpabrack,
  GHCTokenCpabrack,
  GHCTokenCbrack,
  GHCTokenOparen,
  GHCTokenCparen,
  GHCTokenOubxparen,
  GHCTokenCubxparen,
  GHCTokenSemi,
  GHCTokenComma,
  GHCTokenUnderscore,
  GHCTokenBackquote,
  GHCTokenSimpleQuote,

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
