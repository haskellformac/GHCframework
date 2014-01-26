//
//  HFMProject.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Projects are the documents of HfM. This class is a view model...well, actually a (model) controller.
// FIXME: How much of the model can be in Haskell??? Maybe all if we have a proper, seperate model-view object??

#import <Foundation/Foundation.h>


@interface HFMProject : NSDocument

  // FIXME: How do we read and write project files?
  //   Also, how do we deal with the fact that they refer to other files (Haskell, Cabal, etc)?
  //   And we need to be able to read and write those, too.
  // This class must do something about that. NSDocument provides help with reading and writing if using a certain
  // set up, but will that work for us? If we need something custom, we need to reimplement some of NSDocument's
  // functionality.

@end
