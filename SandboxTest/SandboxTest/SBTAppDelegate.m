//
//  SBTAppDelegate.m
//  SandboxTest
//
//  Created by Manuel M T Chakravarty on 15/05/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#include <spawn.h>

#import "SBTAppDelegate.h"

#define USE_SCRIPT  1

#define RUN_EXTERNAL_TOOL "run-external-tool"

#define XCRUN_CLANG "/usr/bin/clang"
#define CLANG "/Library/Developer/CommandLineTools/usr/bin/clang"
#define RUNHASKELL "/usr/bin/runhaskell"

#define C_SOURCE "test.c"
#define C_EXE    "ctest"

#define HS_SOURCE "Hello.hs"

@implementation SBTAppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  NSOpenPanel *panel = [NSOpenPanel openPanel];
  [panel setCanChooseDirectories:YES];
  [panel setAllowsMultipleSelection:YES];
  [panel beginSheetModalForWindow:[self window] completionHandler:^(NSInteger result) {
    NSURL *cwd;
    
    if (result == NSFileHandlingPanelOKButton) {	// Only if not cancelled

      cwd = [panel URL];
      chdir([[cwd path] UTF8String]);

    } else {

      cwd = [NSURL URLWithString:@"/Users/chak/tmp"];
      chdir("/Users/chak/tmp");

    }

    NSString *tmpdir = NSTemporaryDirectory();
    char     *tmpenv = malloc([tmpdir length] + 10);
    sprintf(tmpenv, "TMPDIR=%s", [tmpdir UTF8String]);
    char     *env[]  = {tmpenv};

    if (USE_SCRIPT) {

      NSUserUnixTask *task = runExternalToolTask();
      [task executeWithArguments:@[[cwd path], @XCRUN_CLANG, @"-c", @C_SOURCE] completionHandler:^(NSError *err){}];

    } else {

      char *argv[] = {CLANG, "-c", C_SOURCE, NULL};
      int err = posix_spawn(NULL, CLANG, NULL, NULL, argv, env);
      if (err)
        NSLog(@"unable to spawn clang: error code %d", err);

    }

    if (USE_SCRIPT) {

      NSUserUnixTask *task = runExternalToolTask();
      [task executeWithArguments:@[[cwd path], @XCRUN_CLANG, @"-o", @C_EXE, @C_SOURCE] completionHandler:^(NSError *err){}];

    } else {

      char *argv[] = {CLANG, "-o", C_EXE, C_SOURCE, NULL};
      int err = posix_spawn(NULL, CLANG, NULL, NULL, argv, env);
      if (err)
        NSLog(@"unable to spawn clang: error code %d", err);

    }

    if (USE_SCRIPT) {

      NSUserUnixTask *task = runExternalToolTask();
      task.standardOutput = [NSFileHandle fileHandleWithStandardOutput];
      [task executeWithArguments:@[[cwd path], @"./ctest"] completionHandler:^(NSError *err){}];
//      [task executeWithArguments:@[[cwd path], @C_EXE] completionHandler:^(NSError *err){}];

    } else {

      char *argv[] = {C_EXE, NULL};
      int err = posix_spawn(NULL, C_EXE, NULL, NULL, argv, env);
      if (err)
        NSLog(@"unable to spawn clang-generated executable: error code %d", err);

    }

    {

      char *argv[] = {RUNHASKELL, HS_SOURCE, NULL};
      int err = posix_spawn(NULL, RUNHASKELL, NULL, NULL, argv, NULL);
      if (err)
        NSLog(@"unable to spawn runhaskell: error code %d", err);
      
    }
  }];
}

NSUserUnixTask *runExternalToolTask()
{
  NSFileManager *fm = [NSFileManager defaultManager];
  NSURL *scriptURL = [[fm URLForDirectory:NSApplicationScriptsDirectory
                                 inDomain:NSUserDomainMask
                        appropriateForURL:nil create:NO error:nil]
                      URLByAppendingPathComponent:@RUN_EXTERNAL_TOOL];
  NSUserUnixTask *task = [[NSUserUnixTask alloc] initWithURL:scriptURL error:nil];
// problematic when using multiple tasks; probably need to unbind
//  task.standardOutput = [NSFileHandle fileHandleWithStandardOutput];
//  task.standardError  = [NSFileHandle fileHandleWithStandardError];
  return task;
}

@end
