//
//  HFMProjectViewModelItem.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 13/06/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProjectViewModelItem.h"


@interface HFMProjectViewModelItem ()

/// The project view model that this item belongs to.
///
/// Weak reference as this item is owned by the model.
//
@property (weak, readonly) HFMProjectViewModel *model;

/// Points to the parent item; 'nil' for group items.
///
/// At the moment, the parent relation is immutable. This MAY CHANGE if we implement dragging of folder items in the
/// outline view.
//
@property (weak, readonly) HFMProjectViewModelItem *parent;

/// Mutable set of children of an item.
///
/// This property is initialised to 'nil' and computed on demand.
//
@property (nonatomic) NSMutableArray/*<HFMProjectViewModelItem>*/ *theChildren;

/// If the associated data has been edited and not yet saved, this file wrapper contains the edited data and ought to be
/// saved during the next document save operation.
///
/// If the associated data remains unchanged, this property is 'nil'. Upon saving any changes, this property's value
/// goes into 'fileWrapper' and 'dirtyFileWrapper' is reset to 'nil'.
///
/// Access must be wrapped in '@synchronized(self){..}'.
//
@property NSFileWrapper *dirtyFileWrapper;   // maybe nil


@end


/// Outline view group ids
//
NSString *const kPackageGroupID     = @"Package";
NSString *const kDataGroupID        = @"Data";
NSString *const kExecutableGroupID  = @"Executables";
NSString *const kExtraSourceGroupID = @"Extra sources";


@implementation HFMProjectViewModelItem

@synthesize fileWrapper = _fileWrapper;


#pragma mark Initialisation

+ (instancetype)projectViewModelItemWithGroup:(PVMItemTag)tag
                                   identifier:(NSString *)identifier
                                       parent:(HFMProjectViewModelItem *)parent
                                        model:(HFMProjectViewModel *)model

{
  return [[HFMProjectViewModelItem alloc] initWithGroup:tag
                                             identifier:identifier
                                                 parent:parent
                                                  model:model];
}

- (instancetype)initWithGroup:(PVMItemTag)tag
                   identifier:(NSString *)identifier
                       parent:(HFMProjectViewModelItem *)parent
                        model:(HFMProjectViewModel *)model
{
  self = [super init];
  if (self) {

    _tag         = tag;
    _identifier  = identifier;
    _parent      = parent;
    _model       = model;
    _theChildren = nil;

  }
  return self;
}

#pragma mark -
#pragma mark Mutatation

- (void)updateItemWithData:(NSData *)data
{
  NSFileWrapper *newFileWrapper = [[NSFileWrapper alloc] initRegularFileWithContents:data];
  newFileWrapper.preferredFilename = self.fileWrapper.preferredFilename;

  @synchronized(self) {
    self.dirtyFileWrapper = newFileWrapper;
  }
}


#pragma mark -
#pragma mark Setters and getters

- (void)setIdentifier:(NSString *)identifier
{
  NSLog(@"%s: identifier = %@ : setting of project view model items NOT IMPLEMENTED YET", __func__, identifier);
}

- (NSFileWrapper *)fileWrapper
{
  if (!_fileWrapper) {

    switch (self.tag) {
      case PVMItemTagGroup:
        _fileWrapper = self.model.fileWrapper;
        break;
        
      case PVMItemTagPackage:
        _fileWrapper = [self.model.fileWrapper fileWrappers][self.model.cabalFileName];
        if (!_fileWrapper)
          NSLog(@"%s: cabal file wrapper with filename '%@' disappeared", __func__, self.model.cabalFileName);
        break;

      case PVMItemTagFileGroup: {

        NSCharacterSet *wildcardChars = [NSCharacterSet characterSetWithCharactersInString:@"*"];

          // Chase the path components down the directory hierarchy.
        NSFileWrapper *fileWrapper = (self.parent.fileWrapper) ? self.parent.fileWrapper : self.model.fileWrapper;
        for (NSString *component in [self.identifier pathComponents]) {

          if (!fileWrapper) break;

            // File groups that include wildcards are always filenames (not just directory names) and must disregard the
            // filename wildcard component to determine their directory wrapper.
          NSRange wildcardRange = [component rangeOfCharacterFromSet:wildcardChars];
          if (wildcardRange.location != NSNotFound) break;

          fileWrapper = [fileWrapper fileWrappers][component];

        }
        _fileWrapper = fileWrapper;

        break;
      }

      case PVMItemTagExecutable:
        _fileWrapper = self.model.fileWrapper;
        break;

      case PVMItemTagFile:
      case PVMItemTagFolder:
        _fileWrapper = [self.parent.fileWrapper fileWrappers][self.identifier];
        break;

      default:
        break;
    }

  }
  return _fileWrapper;
}

- (BOOL)isDirty;
{
  return self.dirtyFileWrapper != nil;
}

- (NSArray/*<HFMProjectModelItem>*/ *)children
{
  if (!_theChildren) {

    switch (self.tag) {
      case PVMItemTagGroup:

        if ([self.identifier isEqualToString:kPackageGroupID]) {

            // Package group: 1 child: package header
          _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                          projectViewModelItemWithGroup:PVMItemTagPackage
                                                                             identifier:self.model.identifier
                                                                                 parent:self
                                                                                  model:self.model]];

        } else if ([self.identifier isEqualToString:kExecutableGroupID]) {

            // Executable group: 1 child: executable section
          _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                          projectViewModelItemWithGroup:PVMItemTagExecutable
                                                                             identifier:self.model.executableName
                                                                                 parent:self
                                                                                  model:self.model]];

        } else if ([self.identifier isEqualToString:kExtraSourceGroupID]) {

            // Extra source group: multiple files in a folder hierarchy
          _theChildren = [self childrenFromDictionary:self.model.extraSrcFiles];

        } else if ([self.identifier isEqualToString:kDataGroupID]) {

            // Depending on whether 'data-dir:' is defined, we either...
          if (self.model.dataDir) {   // ...start with a file group or...

              // File group: 1 child: 'data-dir:'
            _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                            projectViewModelItemWithGroup:PVMItemTagFileGroup
                                                            identifier:self.model.dataDir
                                                            parent:self
                                                            model:self.model]];


          } else {                    // ...directly descent into the hierarchy of the data files.

              // Data files group: multiple files in a folder hierarchy
            _theChildren = [self childrenFromDictionary:self.model.dataFiles];

          }

        } else
          _theChildren = [NSMutableArray array];

        break;

      case PVMItemTagExecutable:

          // Depending on whether 'source-dir:' is defined, we either...
        if (self.model.sourceDir) {   // ...start with a file group or...

            // File group: 1 child: 'source-dir:'
          _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                          projectViewModelItemWithGroup:PVMItemTagFileGroup
                                                                             identifier:self.model.sourceDir
                                                                                 parent:self
                                                                                  model:self.model]];


        } else {                    // ...directly display the main module.

          _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                          projectViewModelItemWithGroup:PVMItemTagFile
                                                                             identifier:self.model.modulePath
                                                                                 parent:self
                                                                                  model:self.model]];

        }

        break;

      case PVMItemTagFileGroup:

        if (self.parent.tag == PVMItemTagGroup && [self.parent.identifier isEqualToString:kDataGroupID]) {
            // We are currently in the file group representing the 'data-dir:' field

            // Data files group: multiple files in a folder hierarchy
          _theChildren = [self childrenFromDictionary:self.model.dataFiles];

        } else if (self.parent.tag == PVMItemTagExecutable) {
            // We are currently in the file group representing the 'source-dir:' field

          _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                          projectViewModelItemWithGroup:PVMItemTagFile
                                                                             identifier:self.model.modulePath
                                                                                 parent:self
                                                                                  model:self.model]];

        } else {

          NSLog(@"%s: we are in a 'PVMItemTagFileGroup', but with an unrecognised parent item", __func__);
          _theChildren = [NSMutableArray array];

        }
        break;

      case PVMItemTagFolder:
        _theChildren = [self childrenFromDictionary:[self lookupDictionary]];
        break;

      case PVMItemTagFile:
        _theChildren = [NSMutableArray array];
        break;

      default:
        _theChildren = [NSMutableArray array];
        break;
    }

  }
  return [NSArray arrayWithArray:_theChildren];     // freeze
}

- (NSString *)fileName
{
  NSMutableArray          *path    = [NSMutableArray array];
  HFMProjectViewModelItem *current = self;

  while (current) {
    switch (current.tag) {

      case PVMItemTagGroup:
        break;
        
      case PVMItemTagPackage:
        [path insertObject:self.model.cabalFileName atIndex:0];
        break;

      case PVMItemTagExecutable:
        break;

      case PVMItemTagFileGroup:
      case PVMItemTagFolder:
      case PVMItemTagFile:
        [path insertObject:current.identifier atIndex:0];
        break;
        
      default:
        break;
        
    }
    current = current.parent;
  }

  return [NSString pathWithComponents:path];
}

- (NSFileWrapper *)getUpdatedFileWrapper
{
  NSFileWrapper *updatedFileWrapper;

  @synchronized(self) {
    updatedFileWrapper    = self.dirtyFileWrapper;
    self.dirtyFileWrapper = nil;
  }
  if (updatedFileWrapper)
    _fileWrapper = updatedFileWrapper;

  return updatedFileWrapper;
}


#pragma mark -
#pragma mark Hierarchy

// Given the identifiers of the current item's children, compute its child items.
//
- (NSMutableArray *)childrenFromDictionary:(NSDictionary *)dict
{
  NSMutableArray *children = [NSMutableArray array];

  for (NSString *string in dict) {

    enum PVMItemTag tag = ((NSDictionary *)dict[string]).count == 0 ? PVMItemTagFile : PVMItemTagFolder;
    [children addObject:[HFMProjectViewModelItem projectViewModelItemWithGroup:tag
                                                                    identifier:string
                                                                        parent:self
                                                                         model:self.model]];

  }
  return children;
}

// Compute the identifier dictionary containing the current item's children.
//
- (NSDictionary *)lookupDictionary
{
  NSMutableArray          *path    = [NSMutableArray array];
  HFMProjectViewModelItem *current = self;

  while (current.tag == PVMItemTagFolder || current.tag == PVMItemTagFile) {

    [path insertObject:current.identifier atIndex:0];
    current = current.parent;

  }

  NSDictionary *dict = ([current.identifier isEqualToString:kDataGroupID]
                        || (self.parent.tag == PVMItemTagGroup
                            && [self.parent.identifier isEqualToString:kDataGroupID])) ? current.model.dataFiles :
                       ([current.identifier isEqualToString:kExtraSourceGroupID])      ? current.model.extraSrcFiles
                                                                                       : nil;

  if (dict)
    for (NSString *identifier in path)
      dict = dict[identifier];

  if (!dict) {

    NSLog(@"%s: couldn't find root", __func__);
    return [NSDictionary dictionary];

  } else
    return dict;
}

@end
