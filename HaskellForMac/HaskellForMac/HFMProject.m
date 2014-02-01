//
//  HFMProject.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProject.h"
#import "HFMWindowController.h"
#import "CBLPackage_objc.h"


@interface HFMProject ()

// The Cabal package describing the project. 'CBLPackage' objects are immutable — i.e., whenever package data changes,
// this property will be updated.
//
@property (atomic) CBLPackage *package;

@end


@implementation HFMProject


#pragma mark -
#pragma mark Initialisation

  // FIXME: There are two further init methods that can be overriden and only apply to initialising new documents or
  //  opened-file documents, respectively.
- (instancetype)init
{
  self = [super init];
  return self;
}

  // This initialisation method is invoked if a new document is being created.
- (instancetype)initWithType:(NSString *)typeName error:(NSError *__autoreleasing *)outError
{
  self = [super initWithType:typeName error:outError];
  if (self)
    self.package = [CBLPackage package];
  return self;
}


#pragma mark -
#pragma mark NSDocument methods

+ (BOOL)canConcurrentlyReadDocumentsOfType:(NSString *)typeName
{
#pragma unused(typeName)

  return YES;
}

  // How can we extend that to the Haskell files???
+ (BOOL)autosavesInPlace;
{
  return YES;
}

- (BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError *__autoreleasing *)outError
{
#pragma unused(typeName)

    // FIXME: supposedly, we should disable undo during file reading with '[[self undoManager] disableUndoRegistration]'

  BOOL readSuccess = NO;

  NSString *fileContents = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];

  if (fileContents) {

    self.package = [CBLPackage packageWithString:fileContents];
    if (self.package)
      readSuccess = YES;

  }

  if (!readSuccess && outError)
    *outError = [NSError errorWithDomain:NSCocoaErrorDomain code:NSFileReadUnknownError userInfo:nil];
  return readSuccess;
}

- (NSData *)dataOfType:(NSString *)typeName error:(NSError *__autoreleasing *)outError
{
#pragma unused(typeName)

    // FIXME: do we need to synchronise the data model with the view model?

  NSString *fileContents = [self.package string];
  NSData   *data         = [fileContents dataUsingEncoding:NSUTF8StringEncoding];

  if (!data && outError)
    *outError = [NSError errorWithDomain:NSCocoaErrorDomain code:NSFileWriteUnknownError userInfo:nil];
  return data;
}

- (void)makeWindowControllers
{
  [self addWindowController:[[HFMWindowController alloc] init]];
}

  // FIXME: do we need post-nib-loading code?


@end
