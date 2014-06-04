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
import Data.Version
import qualified
       Distribution.Package            as P
import qualified 
       Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.Text
import Text.ParserCombinators.ReadP

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

-- Generate a proxy class that exposes all Cabal package information that the view model needs to represent.
--
objc_record "CBL" "Package" ''PD.GenericPackageDescription
  [Typed 'emptyGenericPackageDescription, Typed 'parsePackageDescription, 
   'parseOk :> [t|ParseResult PD.GenericPackageDescription -> Maybe PD.GenericPackageDescription|], 
   Typed 'nameAndVersionOfGenericPackage, Typed 'showPackageDescription, Typed 'showPackageIdentifier]
  
      -- Cabal package specification fields
  [ [objcprop| @property (readonly) typename NSString *name; |]    
      ==> ([t| String |],
           [| display . P.packageName |],
           [| \gpd name -> let
                             pd  = PD.packageDescription gpd
                             pid = PD.package pd
                           in
                           gpd {PD.packageDescription = pd {PD.package = pid {P.pkgName = P.PackageName name}}} |])
  , [objcprop| @property (readonly) typename NSString *version; |]
      ==> ([t| String |],
           [| display . P.packageVersion |],
           [| \gpd versionStr -> let
                                   pd  = PD.packageDescription gpd
                                   pid = PD.package pd
                                   --
                                   version = case filter (null . snd) . readP_to_S parseVersion $ versionStr of
                                               [(version, [])] -> version
                                               _               -> Version [0] []
                                 in
                                 gpd {PD.packageDescription = pd {PD.package = pid {P.pkgVersion = version}}} |])
  , [objcprop| @property (readonly) typename NSString *category; |]
      ==> ([t| String |],
           [| PD.category . PD.packageDescription |],
           [| \gpd category -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.category = category}} |])
  ]
  [objcifdecls|
  
    /// Human readable version of the entire package identifier (<package name>-<version>).
    //
    @property (readonly) typename NSString *identifier;
    
    /// Create a package as a new untitled Cabal package.
    //
    + (instancetype)package;
    
    /// Create a package by parsing the given Cabal file string.
    ///
    /// Returns 'nil' in case of a parse error.
    //
    // FIXME: we need to report errors with more information.
    + (instancetype)packageWithString:(typename NSString *)string;
    
    /// Create a new package by updating the name of the existing package.
    ///
    /// The name is *not* validated for well-formedness.
    //
// generated
//    + (instancetype)package:(typename CBLPackage *)package withName:(typename NSString *)name;
    
    /// Create a new package by updating the version of the existing package.
    ///
    /// The version string is *not* validated for well-formedness.
    //
// generated
//    + (instancetype)package:(typename CBLPackage *)package withVersion:(typename NSString *)version;
    
    /// Initialises a package object by parsing the given Cabal file string.
    ///
    /// Returns 'nil' in case of a parse error.
    //
    // FIXME: we need to report errors with more information.
    - (instancetype)initWithString:(typename NSString *)string;
    
    /* Queries
     */
    
    /// Pretty print the package into a Cabal file string.
    //
    - (typename NSString *)string;

  |] 
  [objcimdecls|

    + (instancetype)package
    {
      return [[CBLPackage alloc] initWithGenericPackageDescriptionHsPtr:emptyGenericPackageDescription()];
    }
    
    + (instancetype)packageWithString:(typename NSString *)string
    {
      return [[CBLPackage alloc] initWithString:string];
    }

    - (instancetype)initWithString:(typename NSString *)string
    {
      self = [super init];
      if (self)
      {
    
        typename HsStablePtr *result = parsePackageDescription(string);
        _genericPackageDescriptionHsPtr = parseOk(result);
        if (!_genericPackageDescriptionHsPtr)
          return nil;       // initialisation fails if parsing fails
    
        // FIXME: We need better error handling. Return errors if parsing fails and also return the warnings, even if it
        //        doesn't fail.
      }
      NSLog(@"Loaded Cabal file for package '%@'", nameAndVersionOfGenericPackage(_genericPackageDescriptionHsPtr));
      return self;
    }
    
    // Getter for readonly property
    //
    - (typename NSString *)identifier
    {
      return showPackageIdentifier(self.genericPackageDescriptionHsPtr);
    }
    
    - (typename NSString *)string
    {
      return showPackageDescription(self.genericPackageDescriptionHsPtr);
    }

  |]

objc_emit

foreign export ccall "CBLPackage_initialise" objc_initialise :: IO ()
