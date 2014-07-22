//
//  main.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 16/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "HsFFI.h"


void CBLPackage_initialise(void);


int main(int argc, char *argv[])
{
    // Get the Haskell runtime going.
  hs_init(&argc, &argv);
  CBLPackage_initialise();

    // Launch the Cocoa application.
  return NSApplicationMain(argc, (const char **) argv);
}
