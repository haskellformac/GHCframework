//
//  HFMProject.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  HfM projects are Cabal packages. This class is the controller for the document view. We maintain exactly one window
//  for each open document.

#import <Foundation/Foundation.h>


@interface HFMProject : NSDocument <NSOutlineViewDelegate, NSOutlineViewDataSource>


@end
