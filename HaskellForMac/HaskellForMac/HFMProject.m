//
//  HFMProject.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 17/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProject.h"
#import "HFMWindowController.h"
#import "HFMProjectViewModel.h"


@interface HFMProject ()

/// Our view model proxy to the Haskell-side Cabal package representation.
//
@property (atomic, readonly) HFMProjectViewModel *projectModel;

@end


@implementation HFMProject


#pragma mark -
#pragma mark Initialisation

  // Don't initialise the propery 'projectModel'. This only happens once when we know whether we open an existing
  // document or are creating a new one.
- (instancetype)init
{
  self = [super init];
  return self;
}
  // FIXME: We probably still have to special case the situation where a document was autosaved, but never explicitly saved
  //  to a particular location. Might have to override 'initForURL:withContentsOfURL:ofType:error:'
  //  or 'initWithContentsOfURL:ofType:error:'

  // This initialisation method is invoked if a new document is being created.

- (instancetype)initWithType:(NSString *)typeName error:(NSError *__autoreleasing *)outError
{
  self = [super initWithType:typeName error:outError];
  if (self)
    _projectModel = [HFMProjectViewModel projectViewModel];
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

    _projectModel = [HFMProjectViewModel projectViewModelWithString:fileContents];
    if (self.projectModel)
      readSuccess = YES;

  }

  if (!readSuccess && outError)
    *outError = [NSError errorWithDomain:NSCocoaErrorDomain code:NSFileReadUnknownError userInfo:nil];
  return readSuccess;
}

- (NSData *)dataOfType:(NSString *)typeName error:(NSError *__autoreleasing *)outError
{
#pragma unused(typeName)

  NSString *fileContents = [self.projectModel string];
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


#pragma mark -
#pragma mark NSOutlineViewDataSource protocol methods

- (id)outlineView:(NSOutlineView *)outlineView child:(NSInteger)index ofItem:(id)item
{
#pragma unused(outlineView, index, item)

  if (!item) {
    return [self.projectModel identifier];
  } else
    return @"X"
    ;
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
#pragma unused(outlineView)

  if (!item)
    return YES;
  else
    return NO;
}

- (NSInteger)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
#pragma unused(outlineView)

    // Without a window controller, we do not currently display this document anywhere.
  if (!self.windowControllers.firstObject)
    return 0;

  if (!item)
    return 1;
  else
    return 0;
}

/* Need to implement this if the user should be able to edit the items of the outline view:

- (void)outlineView:(NSOutlineView *)outlineView
     setObjectValue:(id)object
     forTableColumn:(NSTableColumn *)tableColumn
             byItem:(id)item
{
}
 
 */

@end
