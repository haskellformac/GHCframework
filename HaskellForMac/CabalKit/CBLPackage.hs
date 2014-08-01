{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RecordWildCards #-}

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

  -- standard libraries
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Version
import qualified
       Distribution.Package            as P
import Distribution.License            as PD
import qualified 
       Distribution.PackageDescription as PD
import qualified
       Distribution.PackageDescription.Parse as PD 
import Distribution.PackageDescription.Parse hiding (showPackageDescription)
import Distribution.ParseUtils (ppFields) 
import Distribution.Text
import Text.ParserCombinators.ReadP
import Text.PrettyPrint                as PP
import System.FilePath

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

import Debug.Trace

objc_import ["<Cocoa/Cocoa.h>"]


-- Marshalling support
-- -------------------
-- FIXME: this should go into a general library

newtype NSString = NSString (ForeignPtr NSString)
  deriving Typeable   -- needed for now until migrating to new TH

newtype NSMutableArray e = NSMutableArray (ForeignPtr (NSMutableArray e))
  deriving Typeable   -- needed for now until migrating to new TH
newtype NSArray        e = NSArray        (ForeignPtr (NSArray        e))
  deriving Typeable   -- needed for now until migrating to new TH

unsafeFreezeNSMutableArray :: NSMutableArray e -> NSArray e
unsafeFreezeNSMutableArray (NSMutableArray fptr) = NSArray $ castForeignPtr fptr

objc_typecheck

listOfStringToNSArray :: [String] -> IO (NSArray NSString)
listOfStringToNSArray strs
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray NSString|] <: [cexp| [NSMutableArray arrayWithCapacity:50] |])
    ; mapM_ (addElement marr) strs
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr str
      = $(objc ['marr :> Class [t|NSMutableArray NSString|], 'str :> ''String] $ void [cexp| [marr addObject:str] |])

nsArrayTolistOfString :: NSArray NSString -> IO [String]
nsArrayTolistOfString arr
  = do
    { n <- $(objc ['arr :> Class [t|NSArray NSString|]] $ ''Int <: [cexp| (int)arr.count |])
    ; sequence [ $(objc ['arr :> Class [t|NSArray NSString|], 'i :> ''Int] $ 
                     ''String <: [cexp| arr[(typename NSUInteger)i] |]) 
               | i <- [0..n-1]]
    }

objc_marshaller 'listOfStringToNSArray 'nsArrayTolistOfString


-- Haskell helpers
-- ---------------

-- Just the result of parsing if available.
--
parseOk :: ParseResult a -> Maybe a
parseOk (ParseFailed {}) = Nothing
parseOk (ParseOk _ v)    = Just v

-- Just the parser warnings if any.
--
parseWarnings :: ParseResult a -> Maybe String
parseWarnings (ParseFailed {})  = Nothing
parseWarnings (ParseOk warns _) = Just . concat . map show $ warns

-- Just the parse errors if any.
--
parseErrors :: ParseResult a -> Maybe String
parseErrors (ParseFailed err) = Just $ show err
parseErrors (ParseOk _ _)     = Nothing

-- Cabal currently (1.20) doesn't have a function to pretty-print a generic package description. It only handles non-generic
-- ones.
--
-- FIXME: The pretty printed is only partial.
showPackageDescription :: PD.GenericPackageDescription -> String
showPackageDescription gpd
  = PD.showPackageDescription pd ++ showCondExecutables (PD.condExecutables gpd)
  where
    pd = PD.packageDescription gpd
    --
    showCondExecutables []             = ""
    showCondExecutables ((name, cd):_)                -- we currently ignore every one except the first
      = "\nExecutable " ++ name ++ "\n" ++ indent 2 (PP.render (showCondTree showExecutable cd))
      where
        indent n = unlines . map (replicate n ' ' ++) . lines   -- FIXME: inefficient; must be done in Doc
    --
    showCondTree showData (PD.CondNode {..}) = showData condTreeData  -- FIXME: ignoring two fields
    --
    showExecutable = ppFields executableFieldDescrs
    -- showExecutable (Executable {..}) = "  main-is:\t" ++ modulePath  -- FIXME: ignoring two fields

-- Extract the name and version string of a package.
--
nameAndVersionOfGenericPackage :: PD.GenericPackageDescription -> String
nameAndVersionOfGenericPackage = show . PD.package . PD.packageDescription

-- A newly initialised generic package description.
--
emptyGenericPackageDescription :: PD.GenericPackageDescription
emptyGenericPackageDescription = PD.GenericPackageDescription emptyPD [] Nothing [] [] []
  where
    emptyPD = PD.emptyPackageDescription
              { PD.package        = P.PackageIdentifier {pkgName = P.PackageName "Untitled", pkgVersion = Version [0] []}
              , PD.stability      = "experimental"
              , PD.specVersionRaw = Left (Version [1, 6] [])
              , PD.buildType      = Just PD.Simple
              }

-- Pretty print the package identifier.
--
showPackageIdentifier :: PD.GenericPackageDescription -> String
showPackageIdentifier = display . P.packageId

-- Pretty print the Cabal version of the package.
--
cabalVersion :: PD.GenericPackageDescription -> String
cabalVersion gpd = case PD.specVersionRaw . PD.packageDescription $ gpd of
                     Left  vers  -> display vers
                     Right range -> display range

-- Pretty print the build tyoe of the package.
--
buildType :: PD.GenericPackageDescription -> String
buildType = maybe "Simple" display . PD.buildType . PD.packageDescription

type CondExecutable = (String, PD.CondTree PD.ConfVar [P.Dependency] PD.Executable)

-- Get a field of the first executable section.
--
-- The first argument is a default returned if there is no executable section.
---
getCondExecutable :: a -> (CondExecutable -> a) -> PD.GenericPackageDescription -> a
getCondExecutable dft proj gpd
  = case PD.condExecutables $ gpd of
      []      -> dft
      (exe:_) -> proj exe

-- Set a field of the first executable section.
--
-- Create an executable secion if none was present before.
--
setCondExecutable :: PD.GenericPackageDescription -> (CondExecutable -> CondExecutable) -> PD.GenericPackageDescription
setCondExecutable gpd upd
  = case PD.condExecutables gpd of
      []         -> gpd {PD.condExecutables = [upd ("", PD.CondNode PD.emptyExecutable [] [])]}
      (exe:exes) -> gpd {PD.condExecutables = upd exe : exes}

-- Generate a proxy class that exposes all Cabal package information that the view model needs to represent.
--
objc_record "CBL" "Package" ''PD.GenericPackageDescription
  [Typed 'emptyGenericPackageDescription, Typed 'parsePackageDescription, 
   'parseOk       :> [t|ParseResult PD.GenericPackageDescription -> Maybe PD.GenericPackageDescription|], 
   'parseWarnings :> [t|ParseResult PD.GenericPackageDescription -> Maybe String|], 
   'parseErrors   :> [t|ParseResult PD.GenericPackageDescription -> Maybe String|], 
   Typed 'nameAndVersionOfGenericPackage, Typed 'showPackageDescription, Typed 'showPackageIdentifier,
   Typed 'cabalVersion, Typed 'buildType]
  
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
  , [objcprop| @property (readonly) typename NSString *synopsis; |]
      ==> ([t| String |],
           [| PD.synopsis . PD.packageDescription |],
           [| \gpd synopsis -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.synopsis = synopsis}} |])
  , [objcprop| @property (readonly) typename NSString *fullDescription; |]
      ==> ([t| String |],
           [| PD.description . PD.packageDescription |],
           [| \gpd description -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.description = description}} |])
  , [objcprop| @property (readonly) typename NSString *author; |]
      ==> ([t| String |],
           [| PD.author . PD.packageDescription |],
           [| \gpd author -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.author = author}} |])
  , [objcprop| @property (readonly) typename NSString *maintainer; |]
      ==> ([t| String |],
           [| PD.maintainer . PD.packageDescription |],
           [| \gpd maintainer -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.maintainer = maintainer}} |])
  , [objcprop| @property (readonly) typename NSString *copyright; |]
      ==> ([t| String |],
           [| PD.copyright . PD.packageDescription |],
           [| \gpd copyright -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.copyright = copyright}} |])
  , [objcprop| @property (readonly) typename NSString *license; |]
      ==> ([t| String |],
           [| display . PD.license . PD.packageDescription |],
           [| \gpd license ->
                gpd {PD.packageDescription 
                       = (PD.packageDescription gpd) {PD.license = case simpleParse license of
                                                                     Nothing -> PD.UnknownLicense license
                                                                     Just l  -> l}} |])
  , [objcprop| @property (readonly) typename NSString *homepage; |]
      ==> ([t| String |],
           [| PD.homepage . PD.packageDescription |],
           [| \gpd homepage -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.homepage = homepage}} |])
  , [objcprop| @property (readonly) typename NSString *pkgUrl; |]
      ==> ([t| String |],
           [| PD.pkgUrl . PD.packageDescription |],
           [| \gpd pkgUrl -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.pkgUrl = pkgUrl}} |])
  , [objcprop| @property (readonly) typename NSString *bugReports; |]
      ==> ([t| String |],
           [| PD.bugReports . PD.packageDescription |],
           [| \gpd bugReports -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.bugReports = bugReports}} |])
  , [objcprop| @property (readonly) typename NSString *dataDir; |]
      ==> ([t| Maybe String |],
           [| \gpd -> let dir = PD.dataDir . PD.packageDescription $ gpd
                      in
                      if null dir then Nothing else Just dir |],
           [| \gpd maybeDataDir -> gpd {PD.packageDescription 
                                          = (PD.packageDescription gpd) {PD.dataDir = maybe "" id maybeDataDir}} |])
  , [objcprop| @property (readonly) typename NSArray *dataFiles; |]
      ==> ([t| [String] |],
           [| PD.dataFiles . PD.packageDescription |],
           [| \gpd dataFiles -> gpd {PD.packageDescription = (PD.packageDescription gpd) {PD.dataFiles = dataFiles}} |])
  , [objcprop| @property (readonly) typename NSArray *extraSrcFiles; |]
      ==> ([t| [String] |],
           [| PD.extraSrcFiles . PD.packageDescription |],
           [| \gpd extraSrcFiles -> gpd {PD.packageDescription 
                                           = (PD.packageDescription gpd) {PD.extraSrcFiles = extraSrcFiles}} |])
  -- executable section (FIXME: for the moment, we assume, there is exactly one)
  , [objcprop| @property (readonly) typename NSString *executableName; |]
      ==> ([t| String |],
           [| getCondExecutable "" fst |],
           [| \gpd executableName -> setCondExecutable gpd (\(_, cd) -> (executableName, cd)) |])
  , [objcprop| @property (readonly) typename NSString *sourceDir; |]
      ==> ([t| Maybe String |],
           [| \gpd -> let dirs = getCondExecutable [] (PD.hsSourceDirs . PD.buildInfo . PD.condTreeData . snd) gpd
                      in 
                      if null dirs then Nothing else Just $ head dirs |],
           [| (\gpd maybeSourceDir -> 
                setCondExecutable gpd 
                  (\(name, cd) -> 
                    let treeData   = PD.condTreeData cd
                        sourceDirs = case (maybeSourceDir, PD.hsSourceDirs . PD.buildInfo $ treeData) of
                                       (Nothing,  [])     -> []
                                       (Nothing,  _:dirs) -> dirs
                                       (Just dir, [])     -> [dir]
                                       (Just dir, _:dirs) -> dir:dirs
                    in (name, cd {PD.condTreeData 
                                    = treeData {PD.buildInfo = (PD.buildInfo treeData) {PD.hsSourceDirs = sourceDirs}}})))
              :: PD.GenericPackageDescription -> Maybe String -> PD.GenericPackageDescription |])
  , [objcprop| @property (readonly) typename NSString *modulePath; |]
      ==> ([t| String |],
           [| getCondExecutable "" (PD.modulePath . PD.condTreeData . snd) |],
           [| \gpd modulePath -> 
                setCondExecutable gpd 
                  (\(name, cd) -> (name, cd {PD.condTreeData = (PD.condTreeData cd) {PD.modulePath = modulePath}})) |])
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

    void CBLPackage_initialise(void);

    + (void)initialize
    {
      CBLPackage_initialise();
    }

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
        if (!_genericPackageDescriptionHsPtr) {
          NSLog(@"Loading of Cabal file failed: %@", parseErrors(result));
          NSLog(@"...malformed Cabal source: %@", string);
          return nil;       // initialisation fails if parsing fails
        }
        
        typename NSString *warns = parseWarnings(result);
        if (!warns)
          NSLog(@"Cabal parser warnings: %@", warns); 
    
        // FIXME: We need better error handling. Return errors if parsing fails and also return the warnings, even if it
        //        doesn't fail.
      }
      NSLog(@"Loaded Cabal file for package '%@' (Cabal-Version: %@; Build-Type: %@)",
            nameAndVersionOfGenericPackage(_genericPackageDescriptionHsPtr), 
            cabalVersion(_genericPackageDescriptionHsPtr),
            buildType(_genericPackageDescriptionHsPtr));
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
