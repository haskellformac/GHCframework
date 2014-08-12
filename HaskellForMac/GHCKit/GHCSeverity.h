//
//  GHCSeverity.h
//  GHCKit
//
//  Created by Manuel M T Chakravarty on 12/08/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#ifndef GHCKit_GHCSeverity_h
#define GHCKit_GHCSeverity_h

#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, GHCSeverity) {
  GHCSeverityOutput,
  GHCSeverityDump,
  GHCSeverityInteractive,
  GHCSeverityInfo,
  GHCSeverityWarning,
  GHCSeverityError,
  GHCSeverityFatal
};

#endif
