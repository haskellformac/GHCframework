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

// Immutable project properties
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


#pragma mark -
#pragma mark Initialisation

/// Create a new, untitled project model.
//
+ (instancetype)projectViewModel;

/// Create a project model from a Cabal file string.
///
/// Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
+ (instancetype)projectViewModelWithString:(NSString *)string;

/// Initialise a project model from a Cabal file string.
///
/// @Returns 'nil' in case of a parse error.
//
// FIXME: we need to report errors with more information.
- (instancetype)initWithString:(NSString *)string;


#pragma mark -
#pragma mark Project serialisation

/// Serialise the project data into a string.
//
- (NSString *)string;


@end
