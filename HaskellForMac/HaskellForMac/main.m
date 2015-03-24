//
//  main.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 16/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "GHC/GHC.h"
  // void hs_init_with_rtsopts (int *argc, char **argv[]);


void CloudcelerateKit_initialise(void);

int main(int argc, char *argv[])
{

    // Make sure GHC is properly relocated.
  NSString *relocateRelative = @"Contents/Frameworks/GHC.framework/Versions/Current/Executables/RelocateGHC";
  NSString *relocate         = [[NSBundle mainBundle].bundlePath stringByAppendingPathComponent:relocateRelative];
  NSTask   *relocateTask     = [NSTask launchedTaskWithLaunchPath:relocate arguments: @[]];
  [relocateTask waitUntilExit];
  if ([relocateTask terminationStatus] != 0) {
    NSLog(@"relocation of GHC failed");
  }
  relocateTask = nil;

    // Get the Haskell runtime going.
  dispatch_sync(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
    int my_argc = argc;
    char **my_argv = argv;
    hs_init(&my_argc, &my_argv);
    /*
    char **my_argv = malloc(sizeof(char*) * (unsigned long)my_argc);
    for (int i = 0; i < argc; i++) { my_argv[i] = argv[i]; }
    my_argv[argc]   = "+RTS";
    my_argv[argc+1] = "-S/Users/chak/Desktop/GHC.stats.txt";
    my_argv[argc+2] = "-qg0";
    my_argv[argc+3] = "-qb1";
    hs_init_with_rtsopts(&my_argc, &my_argv);
     */
  });

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
