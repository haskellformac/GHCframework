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
import qualified
       Distribution.Package            as P
import qualified 
       Distribution.PackageDescription as PD
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
nameAndVersionOfGenericPackage :: PD.GenericPackageDescription -> String
nameAndVersionOfGenericPackage = show . PD.package . PD.packageDescription

-- A newly initialised generic package description.
--
emptyGenericPackageDescription :: PD.GenericPackageDescription
emptyGenericPackageDescription = PD.GenericPackageDescription PD.emptyPackageDescription [] Nothing [] [] []

-- Pretty print the package identifier.
--
showPackageIdentifier :: PD.GenericPackageDescription -> String
showPackageIdentifier = display . P.packageId

-- Projections

packageName :: PD.GenericPackageDescription -> String
packageName = display . P.packageName

packageVersion :: PD.GenericPackageDescription -> String
packageVersion = display . P.packageVersion

-- Update functions

updateName :: PD.GenericPackageDescription -> String -> PD.GenericPackageDescription
updateName gpd name = gpd {PD.packageDescription = pd {PD.package = pid {P.pkgName = P.PackageName name}}}
  where
    pd  = PD.packageDescription gpd
    pid = PD.package pd


-- Objective-C class interface
-- ---------------------------

objc_interface [cunit|

@interface CBLPackage : NSObject

/* Properties
 */
 
/// Human readable version of the entire package identifier (<package name>-<version>).
//
@property (readonly) typename NSString *identifier;

// Cabal package specification fields
//
@property (readonly) typename NSString *name;
@property (readonly) typename NSString *version;


/* Initialisation
 */

/// Create a package as a new untitled Cabal package.
//
+ (typename instancetype)package;

/// Create a package by parsing the given Cabal file string.
///
/// Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
+ (typename instancetype)packageWithString:(typename NSString *)string;

/// Create a new package by updating the name of the existing package.
//
+ (typename instancetype)package:(typename CBLPackage *)package withNewName:(typename NSString *)name;

/// Initialises a package object by parsing the given Cabal file string.
///
/// Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
- (typename instancetype)initWithString:(typename NSString *)string;

/// Initialise a new package from an existing package, but updating its name.
//
- (typename instancetype)initWithGenericPackageDescription:(typename HsStablePtr)packageDescriptionPtr;


/* Queries
 */
  
/// Pretty print the package into a Cabal file string.
//
- (typename NSString *)string;

@end
|]


-- Objective-C class implementation
-- --------------------------------

objc_implementation 
  ['emptyGenericPackageDescription, 'parsePackageDescription, 'parseOk, 'nameAndVersionOfGenericPackage, 
   'showPackageDescription, 'showPackageIdentifier, 'packageName, 'packageVersion, 'updateName] 
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

+ (typename instancetype)package:(typename CBLPackage *)package withNewName:(typename NSString *)name
{
  return [[CBLPackage alloc] initWithGenericPackageDescription:updateName(package.genericPackageDescription, name)];
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

- (typename instancetype)initWithGenericPackageDescription:(typename HsStablePtr)pd
{
  self = [super init];
  if (self)
  {
    _genericPackageDescription = pd;
  }
  NSLog(@"Updated Cabal package '%@'", nameAndVersionOfGenericPackage(_genericPackageDescription));
  return self;
}

- (void)dealloc
{
  hs_free_stable_ptr(_genericPackageDescription);
}


/* Getters
 */

- (typename NSString *)identifier
{
  return showPackageIdentifier(self.genericPackageDescription);
}

- (typename NSString *)name
{
  return packageName(self.genericPackageDescription);
}

- (typename NSString *)version
{
  return packageVersion(self.genericPackageDescription);
}


/* Queries
 */
 
- (typename NSString *)string
{
  return showPackageDescription(self.genericPackageDescription);
}

@end
|]


objc_emit

foreign export ccall "CBLPackage_initialise" objc_initialise :: IO ()
