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
//  The various data elements stored in a cabal file are available through project properties defined below. All
//  file-related properties are also represented in the forest rooted in the property `groupItems`. More specifically,
//  the forest rooted in `groupItems` is initialised from those file-related project properties after a project has
//  been loaded. When saving, all dirty items of the forest of `ProjectItem`s update the corresponding project
//  properties before generating a new Cabal file that is being made persistent.
//
//  NB: This view model owns all the objects representing items for the outline view. The outline view by itself will
//      *not* keep them alive.

#import <Foundation/Foundation.h>

// Forward declaration to avoid cycles through the bridging header.
@class ProjectItemGroups;


@interface HFMProjectViewModel : NSObject

/// The file wrapper of the project document and of the name of the Cabal file inside the document.
///
@property (readonly) NSFileWrapper *fileWrapper;
@property (readonly) NSString      *cabalFileName;
@property (readonly) NSURL         *documentURL;             // URL of the represented document at load time.

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
@property                       NSString     *license;
@property                       NSString     *homepage;
@property                       NSString     *pkgUrl;
@property                       NSString     *bugReports;
@property                       NSString     *dataDir;         // optional (maybe 'nil')
@property                       NSDictionary *dataFiles;       // Folders as tree of dictionaries (leafs = empty dicts)
@property                       NSDictionary *extraSrcFiles;   // Folders as tree of dictionaries (leafs = empty dicts)
// executable section
@property                       NSString     *executableName;
@property                       NSString     *sourceDir;       // optional (maybe 'nil')  FIXME: Issue #82: generalise to array of dirs
@property                       NSDictionary *modules;         // Folders as tree of dictionaries (leafs = empty dicts)
                                                               // Combines Cabal's 'modulePath' and 'otherModules',
                                                               // where only 'modulePath' (main module) has got a suffix.

/// View model group items (they are owned by the view model and keep all active project view model items alive)
///
/// The groups are fixed; hence, the 'readonly'. However, the contents of the group items is mutable.
///
@property (readonly) ProjectItemGroups *groupItems;


#pragma mark -
#pragma mark Informative class methods

/// The file extension of Cabal files.
///
+ (NSString *)cabalFileExtension;

/// The file extension of Haskell files.
///
+ (NSString *)haskellFileExtension;

/// The file extension of Haskell playground files.
///
+ (NSString *)haskellPlaygroundFileExtension;

/// The file extension of plain text files.
///
+ (NSString *)textFileExtension;



#pragma mark -
#pragma mark Initialisation

/// Create a project model for the project given by the file wrappers. The 'cabalFileWrapper' refers to the Cabal
/// file of the project. The latter *may* be 'nil', in which case, it needs to be created.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
+ (instancetype)projectViewModelWithProjectFileWrapper:(NSFileWrapper *)projectFileWrapper
                                      cabalFileWrapper:(NSFileWrapper *)cabalFileWrapper
                                           documentURL:(NSURL *)documentURL;

/// Initialise a project model for the project given by the file wrappers. The 'cabalFileWrapper' refers to the Cabal
/// file of the project. The latter *may* be 'nil', in which case, it needs to be created.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
- (instancetype)initWithProjectFileWrapper:(NSFileWrapper *)projectFileWrapper
                          cabalFileWrapper:(NSFileWrapper *)cabalFileWrapper
                               documentURL:(NSURL *)documentURL;

/// Configure an initialised, but still empty project as a default Haskell project.
//
- (void)createProjectForURL:(NSURL *)url;


#pragma mark -
#pragma mark Project serialisation

/// Turn the entire project document into a directory file wrapper.
///
/// To support incremental updating of the persistent representation, we reuse all file wrappers whose contents hasn't
/// changed.
//
- (NSFileWrapper *)fileWrapperWithError:(NSError *__autoreleasing *)outError;

/// Serialise the Cabal file information in the model to disk.
///
/// Usually, we save the entire document with `-[HFMProjectViewModel fileWrapperWithError:]`. However, after direct
/// file system changes (e.g., to copy in existing files), we need to flush out the Cabal file only (as writing the
/// whole document at this point would fail due to the file system changes).
///
- (BOOL)writeCabalFileWithError:(NSError *__autoreleasing *)error;

@end
