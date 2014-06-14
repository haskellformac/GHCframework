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

/// The name of the Cabal file *relative* to the root document.
///
/// NB: The basename of the file may be different from the project identifier.
//
@property (nonatomic, readonly) NSString *cabalFileName;

// Project properties
//
@property (readonly, nonatomic) NSString *identifier;    // Project identifier (i.e., package name & version) — lazy
@property                       NSString *name;          // Package name
@property                       NSString *version;       // Package version; format as def by 'Data.Version.showVersion'
@property                       NSString *category;
@property                       NSString *synopsis;
@property                       NSString *fullDescription;
@property                       NSString *author;
@property                       NSString *maintainer;
@property                       NSString *copyright;
@property                       NSString *homepage;
@property                       NSString *pkgUrl;
@property                       NSString *bugReports;
// executable section
@property                       NSString *executableName;
@property                       NSString *modulePath;

// View model group items (they are owned by the view model and keep all active project view model items alive)
//
// The groups are fixed; hence, the 'readonly'. However, the contents of the group items is mutable.
//
@property (readonly) NSArray/*<HFMProjectViewModelItem>*/ *groupItems;


#pragma mark -
#pragma mark Initialisation

/// Create a new, untitled project model.
//
+ (instancetype)projectViewModelWithCabalFileName:(NSString *)cabalFileName;

/// Create a project model from a Cabal file string.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
+ (instancetype)projectViewModelWithCabalFileName:(NSString *)cabalFileName string:(NSString *)string;

/// Initialise a new, untitled model.
///
// FIXME: we need to report errors with more information.
- (instancetype)initWithCabalFileName:(NSString *)cabalFileName;

/// Initialise a project model from a Cabal file string.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
- (instancetype)initWithCabalFileName:(NSString *)cabalFileName string:(NSString *)string;


#pragma mark -
#pragma mark Project serialisation

/// Serialise the project data into a string.
//
- (NSString *)string;


@end
