//
//  GHC.h
//  GHC
//
//  Created by Manuel M T Chakravarty on 23/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import <Cocoa/Cocoa.h>

//! Project version number for GHC.
FOUNDATION_EXPORT double GHCVersionNumber;

//! Project version string for GHC.
FOUNDATION_EXPORT const unsigned char GHCVersionString[];

// In this header, you should import all the public headers of your framework using statements like #import <GHC/PublicHeader.h>

// The interface of the GHC framework module is the Haskell C FFI.
#import "GHC/HsFFI.h"
