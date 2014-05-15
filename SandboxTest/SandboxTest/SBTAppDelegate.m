//
//  SBTAppDelegate.m
//  SandboxTest
//
//  Created by Manuel M T Chakravarty on 15/05/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#include <spawn.h>

#import "SBTAppDelegate.h"


@implementation SBTAppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  chdir("/Users/chak/tmp");

  char *argv[] = {"/usr/bin/clang", "-c", "test.c", NULL};
  int err = posix_spawn(NULL, "/usr/bin/clang", NULL, NULL, argv, NULL);
  if (err)
    NSLog(@"unable to spawn clang: error code %d", err);
}

@end
