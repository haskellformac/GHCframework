//
//  HFMProjectViewModel.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/02/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This class implements the view model interfacing project views (and their controllers) with the Haskell-side model
//  object encapsualted by the HFMCBL target.
//
//  NB: This view model owns all the objects representing items for the outline view. The outline view by itself will
//      *not* keep them alive.

#import <Foundation/Foundation.h>


@interface HFMProjectViewModel : NSObject

// Immutable project properties
//
@property (readonly, nonatomic) NSString *identifier;    // Project identifier (i.e., package name & version) — lazy
@property (readonly)            NSString *name;          // Package name
@property (readonly)            NSString *version;       // Package version; format as def by 'Data.Version.showVersion'


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
