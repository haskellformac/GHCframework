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


// XIB file ids
//
NSString *kCabalCellID = @"cabalCellID";


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


#pragma mark -
#pragma mark NSOutlineViewDelegate protocol methods

- (NSTableCellView *)outlineView:(NSOutlineView *)outlineView
              viewForTableColumn:(NSTableColumn *)tableColumn
                            item:(NSString *)name
{
#pragma unused(tableColumn)     // there is only one column

  NSTableCellView *cell = [outlineView makeViewWithIdentifier:kCabalCellID owner:self];
  cell.textField.stringValue = name;
  return cell;
}


#pragma mark -
#pragma mark NSOutlineViewDataSource protocol methods

- (id)outlineView:(NSOutlineView *)outlineView child:(NSInteger)index ofItem:(id)item
{
#pragma unused(outlineView, index, item)

  if (!item)
    return @"ROOT";  // [self.package identifier];  FIXME: it's probably not yet initialised
  else
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
