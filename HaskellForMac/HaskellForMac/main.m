//
//  main.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 16/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "GHC/GHC.h"


void CloudcelerateKit_initialise(void);

int main(int argc, char *argv[])
{

    // Make sure GHC is properly relocated.
  NSString *relocate = [[NSBundle mainBundle].bundlePath stringByAppendingPathComponent:@"Contents/MacOS/relocateGHC"];
  system([relocate cStringUsingEncoding:NSUTF8StringEncoding]);

    // Get the Haskell runtime going.
  hs_init(&argc, &argv);

    // Receipt validation
  NSError  *error;
  NSBundle *bundle     = [NSBundle mainBundle];
  NSURL    *receiptURL = [bundle appStoreReceiptURL];
  if (!receiptURL)
    NSLog(@"Where is it?");
  if (![receiptURL checkResourceIsReachableAndReturnError:&error]) {

    NSLog(@"Can't reach it: %@", error);
//    exit(173);    // bye bye

  }

    // The object in the 'Cloudcelerate' module has class methods calling into Haskell; hence, we need to initialise here.
  CloudcelerateKit_initialise();

    // Launch the Cocoa application.
  return NSApplicationMain(argc, (const char **) argv);
}
