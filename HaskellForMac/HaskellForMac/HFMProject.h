//
//  HFMProject.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  HfM projects are an 'NSDocument' representation of Cabal packages. They serve as a document model object for the
//  in-app representation of the actual project data on external storage. We maintain exactly one window for each open
//  document; hence, we will only ever have one window controller for each document.
//
//  The persistent representation (model) of the project structure is its Cabal file. In the view, the project structure
//  is represented in the outline view. To connect the two, this view model class serves as the data source for the
//  outline view.

#import <Foundation/Foundation.h>
#import "HFMProjectViewModel.h"


@interface HFMProject : NSDocument <NSOutlineViewDataSource>

/// Our view model proxy to the Haskell-side Cabal package representation.
//
@property (atomic, readonly) HFMProjectViewModel *projectModel;

/// An array listing all available group section names.
//
@property (nonatomic, readonly) NSArray *outlineGroups;


@end
