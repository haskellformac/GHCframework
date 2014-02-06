{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : CBLPackage
-- Copyright   : [2014] Manuel M T Chakravarty
-- License     : All rights reserved
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
--
-- This module implements the Objective-C to Haskell bridge for Cabal packages. On the Objective-C side, a Cabal
-- package is an immutable object.

module CBLPackage () where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- standard libraries
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]


-- Haskell helpers
-- ---------------

-- Drop detailed errors and warnings for now. FIXME: this is not convenient for the user.
--
parseOk :: ParseResult a -> Maybe a
parseOk (ParseFailed {}) = Nothing
parseOk (ParseOk _ v)    = Just v

-- Extract the name and version string of a package.
--
nameAndVersionOfGenericPackage :: GenericPackageDescription -> String
nameAndVersionOfGenericPackage = show . package . packageDescription

-- A newly initialised generic package description.
--
emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = GenericPackageDescription emptyPackageDescription [] Nothing [] [] []

-- Pretty print the package identifier.
--
showPackageIdentifier :: GenericPackageDescription -> String
showPackageIdentifier = display . packageId


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

@interface CBLPackage : NSObject

// Create a package as a newly untitled Cabal package.
//
+ (typename instancetype)package;

// Create a package by parsing the given Cabal file string.
//
// Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
+ (typename instancetype)packageWithString:(typename NSString *)string;

// Initialises a package object by parsing the given Cabal file string.
//
// Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
- (typename instancetype)initWithString:(typename NSString *)string;


/* Queries
 * *******
 */
  
// Pretty print the package into a Cabal file string.
//
- (typename NSString *)string;

// Readable package identifier (<package name>-<version>).
//
- (typename NSString *)identifier;

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation 
  ['emptyGenericPackageDescription, 'parsePackageDescription, 'parseOk, 'nameAndVersionOfGenericPackage, 
   'showPackageDescription, 'showPackageIdentifier] 
  [cunit|

@interface CBLPackage ()

@property (readonly, assign, nonatomic) typename HsStablePtr genericPackageDescription;

@end

@implementation CBLPackage

+ (typename instancetype)package
{
  return [[CBLPackage alloc] init];
}

+ (typename instancetype)packageWithString:(typename NSString *)string
{
  return [[CBLPackage alloc] initWithString:string];
}

- (typename instancetype)init
{
  self = [super init];
  if (self)
  {
    _genericPackageDescription = emptyGenericPackageDescription();
  }
  NSLog(@"Initialised new Cabal package '%@'", nameAndVersionOfGenericPackage(_genericPackageDescription));
  return self;
}

- (typename instancetype)initWithString:(typename NSString *)string
{
  self = [super init];
  if (self)
  {

    typename HsStablePtr *result = parsePackageDescription(string);
    _genericPackageDescription = parseOk(result);
    if (!_genericPackageDescription)
      return nil;       // initialisation fails if parsing fails

    // FIXME: We need better error handling. Return errors if parsing fails and also return the warnings, even if it
    //        doesn't fail.
  }
  NSLog(@"Loaded Cabal file for package '%@'", nameAndVersionOfGenericPackage(_genericPackageDescription));
  return self;
}

- (void)dealloc
{
  hs_free_stable_ptr(_genericPackageDescription);
}

- (typename NSString *)string
{
  return showPackageDescription(self.genericPackageDescription);
}

- (typename NSString *)identifier
{
  return showPackageIdentifier(self.genericPackageDescription);
}

@end
|]


objc_emit

foreign export ccall "CBLPackage_initialise" objc_initialise :: IO ()
