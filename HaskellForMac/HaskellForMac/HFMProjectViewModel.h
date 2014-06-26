//
//  HFMProjectViewModel.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/02/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This class implements the view model interfacing project views (and their controllers) with the Haskell-side model
//  object encapsulted by the HFMCBL target. The view model is mutable to conform to KVC, whereas the Haskell-side
//  model is immutable. This class provides the adaptation.
//
//  NB: This view model owns all the objects representing items for the outline view. The outline view by itself will
//      *not* keep them alive.

#import <Foundation/Foundation.h>


@interface HFMProjectViewModel : NSObject

/// The file wrapper of the project document and of the Cabal file inside the document as well as the project URL.
//
@property (readonly) NSFileWrapper *fileWrapper;
@property (readonly) NSFileWrapper *cabalFileWrapper;

// Project properties
//
@property (readonly, nonatomic) NSString     *identifier;    // Project identifier (i.e., package name & version) — lazy
@property                       NSString     *name;          // Package name
@property                       NSString     *version;       // Package version; format as def by 'Data.Version.showVersion'
@property                       NSString     *category;
@property                       NSString     *synopsis;
@property                       NSString     *fullDescription;
@property                       NSString     *author;
@property                       NSString     *maintainer;
@property                       NSString     *copyright;
@property                       NSString     *homepage;
@property                       NSString     *pkgUrl;
@property                       NSString     *bugReports;
@property                       NSString     *dataDir;         // optional (maybe 'nil')
@property                       NSDictionary *dataFiles;       // Folders as tree of dictionaries (leafs = empty dicts)
@property                       NSDictionary *extraSrcFiles;   // Folders as tree of dictionaries (leafs = empty dicts)
// executable section
@property                       NSString     *executableName;
@property                       NSString     *sourceDir;       // optional (maybe 'nil')  FIXME: Issue #82: generalise to array of dirs
@property                       NSString     *modulePath;

// View model group items (they are owned by the view model and keep all active project view model items alive)
//
// The groups are fixed; hence, the 'readonly'. However, the contents of the group items is mutable.
//
@property (readonly) NSArray/*<HFMProjectViewModelItem>*/ *groupItems;


#pragma mark -
#pragma mark Informative class methods

/// The file extension of Cabal files.
//
+ (NSString *)cabalFileExtension;

#pragma mark -
#pragma mark Initialisation

/// Create a project model for the project given by the file wrappers. The 'cabalFileWrapper' refers to the Cabal
/// file of the project. The latter *may* be 'nil', in which case, it needs to be created.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
+ (instancetype)projectViewModelWithProjectFileWrapper:(NSFileWrapper *)projectFileWrapper
                                      cabalFileWrapper:(NSFileWrapper *)cabalFileWrapper;

/// Initialise a project model for the project given by the file wrappers. The 'cabalFileWrapper' refers to the Cabal
/// file of the project. The latter *may* be 'nil', in which case, it needs to be created.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
- (instancetype)initWithProjectFileWrapper:(NSFileWrapper *)projectFileWrapper
                          cabalFileWrapper:(NSFileWrapper *)cabalFileWrapper;


#pragma mark -
#pragma mark Project serialisation

/// Serialise the project data into a string.
//
- (NSString *)string;


@end
