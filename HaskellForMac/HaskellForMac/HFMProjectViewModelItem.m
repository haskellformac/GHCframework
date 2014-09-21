//
//  HFMProjectViewModelItem.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 13/06/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "Haskell-Swift.h"
#import "HFMProjectViewModelItem.h"


@interface HFMProjectViewModelItem ()

/// The project view model that this item belongs to.
///
/// Weak reference as this item is owned by the model.
///
@property (weak, readonly) HFMProjectViewModel *model;

/// Points to the parent item; 'nil' for group items.
///
/// At the moment, the parent relation is immutable. This MAY CHANGE if we implement dragging of items in the
/// outline view.
///
@property (weak, readonly) HFMProjectViewModelItem *parent;

/// Mutable set of children of an item.
///
/// This property is initialised to 'nil' and computed on demand.
///
@property (nonatomic) NSMutableArray/*<HFMProjectViewModelItem>*/ *theChildren;

/// If the associated data has been edited and not yet saved, this file wrapper contains the edited data and ought to be
/// saved during the next document save operation.
///
/// If the associated data remains unchanged, this property is 'nil'. Upon saving any changes, this property's value
/// goes into 'fileWrapper' and 'dirtyFileWrapper' is reset to 'nil'.
///
/// Access must be wrapped in '@synchronized(self){..}'.
///
/// NB: This property is only relevant for non-directory file wrappers (as they are immutable). In contrast, directory
///     file wrappers are always modified in place. (In other words, dirty here refers to the item being dirty wrt. to
///     keeping the file wrapper tree in sync — it makes no statement about whether any data needs to be saved to the
///     file system. We leave that to the file wrappers to figure out.
///
@property NSFileWrapper *dirtyFileWrapper;   // maybe nil

@end


/// Outline view group ids
//
NSString *const kPackageGroupID     = @"Project information";
NSString *const kDataGroupID        = @"Additional files";
NSString *const kExecutableGroupID  = @"Programs";
NSString *const kExtraSourceGroupID = @"Non-Haskell sources";


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
#pragma mark Setters and getters

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
      case PVMItemTagMainFile:
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

- (BOOL)isEmptyFolder
{
  if (self.tag != PVMItemTagFolder && self.tag != PVMItemTagFileGroup) return NO;
  for (HFMProjectViewModelItem *child in self.children)
    if (!child.isEmptyFolder) return NO;
  
  return YES;
}

- (NSUInteger)index
{
  if (!self.parent)
    return [self.model.groupItems indexOfObject:self];
  else
    return [self.parent.children indexOfObject:self];
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
          _theChildren = [self childrenFromDictionary:self.model.extraSrcFiles asSourceModules:NO];

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
            _theChildren = [self childrenFromDictionary:self.model.dataFiles asSourceModules:NO];

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


        } else {                    // ...directly display the main module and other modules.

          _theChildren = [self childrenFromDictionary:self.model.modules asSourceModules:YES];

        }

        break;

      case PVMItemTagFileGroup:

        if (self.parent.tag == PVMItemTagGroup && [self.parent.identifier isEqualToString:kDataGroupID]) {
            // We are currently in the file group representing the 'data-dir:' field

            // Data files group: multiple files in a folder hierarchy
          _theChildren = [self childrenFromDictionary:self.model.dataFiles asSourceModules:NO];

        } else if (self.parent.tag == PVMItemTagExecutable) {
            // We are currently in the file group representing the 'source-dir:' field

          _theChildren = [self childrenFromDictionary:self.model.modules asSourceModules:YES];

        } else {

          NSLog(@"%s: we are in a 'PVMItemTagFileGroup', but with an unrecognised parent item", __func__);
          _theChildren = [NSMutableArray array];

        }
        break;

      case PVMItemTagFolder:
      {
        BOOL asSourceModules;
        NSDictionary *dict = [self lookupDictionaryMightBeSources:&asSourceModules];
        _theChildren = [self childrenFromDictionary:dict asSourceModules:asSourceModules];
        break;
      }

      case PVMItemTagFile:
      case PVMItemTagMainFile:
        _theChildren = [NSMutableArray array];
        break;

      default:
        _theChildren = [NSMutableArray array];
        break;
    }

  }
  return [NSArray arrayWithArray:_theChildren];     // freeze
}

- (NSString *)filePath
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
      case PVMItemTagMainFile:
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

- (NSString *)string
{
  NSData *data;

  @synchronized(self) {
      // Obtain the most recent version of the contents.
    data = (self.dirtyFileWrapper)
           ? [self.dirtyFileWrapper regularFileContents]
           : [self.fileWrapper regularFileContents];
  }
  return [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
}

- (void)setString:(NSString *)string
{
  NSData        *data              = [string dataUsingEncoding:NSUTF8StringEncoding];
  NSFileWrapper *newFileWrapper    = [[NSFileWrapper alloc] initRegularFileWithContents:data];
  NSString      *preferredFilename = (self.fileWrapper) ? self.fileWrapper.preferredFilename : self.identifier;

  if (!preferredFilename) {
    NSLog(@"%s: cannot determine filename", __func__);
    return;
  }
  newFileWrapper.preferredFilename = preferredFilename;

  @synchronized(self) {
    self.dirtyFileWrapper = newFileWrapper;
  }
}

- (NSAttributedString *)attributedString
{
  return [[NSAttributedString alloc] initWithString:self.string];
}

- (void)setAttributedString:(NSAttributedString *)attributedString
{
  self.string = [attributedString string];
}


#pragma mark -
#pragma mark Edits

- (void)touchFileWrapper
{
  if (!self.fileWrapper) {
    self.dirtyFileWrapper = [[NSFileWrapper alloc] initRegularFileWithContents:[NSData data]];
    self.dirtyFileWrapper.preferredFilename = self.identifier;
  }
}

- (BOOL)newHaskellSourceAtIndex:(NSUInteger)index
{
  if ((self.tag != PVMItemTagGroup || ![self.identifier isEqualToString:kExtraSourceGroupID])
      && self.tag != PVMItemTagExecutable
      && self.tag != PVMItemTagFileGroup
      && self.tag != PVMItemTagFolder) return NO;
  if (self.children.count < index) return NO;

  NSMutableArray *usedNames = [NSMutableArray array];
  for (HFMProjectViewModelItem *child in self.theChildren) {
    [usedNames addObject:[child.identifier stringByDeletingPathExtension]];
  }
  NSString *identifier = [[Swift nextNameOf:@"NewSource" usedNames:usedNames]
                          stringByAppendingPathExtension:[HFMProjectViewModel haskellFileExtension]];
  HFMProjectViewModelItem *newChild = [HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagFile
                                                                                  identifier:identifier
                                                                                      parent:self
                                                                                       model:self.model];
  [self.theChildren insertObject:newChild atIndex:index];
  return YES;
}

  // NB: We could avoid passing in the child item if we could easily compute it from the file wrapper. This is not
  //     convenient currently as the children computation computes all children. MAYBE TODO: refactor to be able to
  //     do this for individual child items.
- (void)addChild:(HFMProjectViewModelItem *)child fileWrapper:(NSFileWrapper *)childFileWrapper
{
  @synchronized(self) {

      // Obtain the most recent version of the file wrapper.
    NSFileWrapper *currentFileWrapper = (self.dirtyFileWrapper) ? self.dirtyFileWrapper : self.fileWrapper;
    [currentFileWrapper addFileWrapper:childFileWrapper];
    self.dirtyFileWrapper = currentFileWrapper;
      // NB: If the current one was the original 'self.fileWrapper', we still need to assign to
      //     'self.dirtyFileWrapper'; otherwise, the dirty status is not represented properly.

  }
  [self children];    // Make sure '_theChildren' is initialised
  [_theChildren addObject:child];
}

- (BOOL)remove
{
  if (self.tag == PVMItemTagMainFile) return NO;      // We cannot delete the Main file

  HFMProjectViewModelItem *mainItem = [self containedMainFile];
  if (mainItem) {
      // FIXME: We need to move the main file to be our sibling. Then, delete as usual. (Issue #164)
    NSLog(@"Cannot delete folders containing the main file yet (Issue #164)");
    return NO;
  }

    // Remove this item from the hierarchy.
  return [self.parent removeChild:self];
}

- (BOOL)removeChild:(HFMProjectViewModelItem *)childItem
{
    // Remove the model view item.
  if (![self.children containsObject:childItem]) return NO;
  [_theChildren removeObject:childItem];

    // Remove the matching file wrapper.
  @synchronized(self) {

    [self.fileWrapper removeFileWrapper:childItem.fileWrapper];
    self.dirtyFileWrapper = self.fileWrapper;
      // NB: We still need to assign to 'self.dirtyFileWrapper'; otherwise, the new dirty status is not represented properly.

  }

  return YES;
}

- (NSString*)renameTo:(NSString *)newIdentifier
{
  NSFileWrapper *parentFileWrapper = (self.parent.fileWrapper) ? self.parent.fileWrapper : self.model.fileWrapper;

  self.fileWrapper.preferredFilename = newIdentifier;
  NSString *actualIdentifier         = [parentFileWrapper keyForFileWrapper:self.fileWrapper];
  self.identifier                    = actualIdentifier;

  return ([newIdentifier isEqualToString:actualIdentifier] ? nil : actualIdentifier);
}


#pragma mark -
#pragma mark File wrapper update

void updateFileWrappers(NSFileWrapper *parentFileWrapper, NSArray *items)
{
  for (HFMProjectViewModelItem *item in items)
    updateFileWrapper(parentFileWrapper, item);
}

void updateFileWrapper(NSFileWrapper *parentFileWrapper, HFMProjectViewModelItem *item)
{
  switch (item.tag) {
    case PVMItemTagGroup:
    case PVMItemTagExecutable:
      updateFileWrappers(parentFileWrapper, item.children);
      break;

    case PVMItemTagFileGroup: {
      NSFileWrapper *itemFileWrapper = parentFileWrapper;
      for (NSString *component in [item.filePath pathComponents]) {

        itemFileWrapper = itemFileWrapper.fileWrappers[component];
        if (!itemFileWrapper) {
          NSLog(@"%s: missing file wrapper in group '%@'", __func__, item.filePath);
        }
        if (![itemFileWrapper isDirectory]) {
          NSLog(@"%s: file group item '%@', but no directory wrapper '%@'", __func__, item.filePath, component);
          break;
        }

      }

      updateFileWrappers(itemFileWrapper, item.children);
      break;
    }
      
    case PVMItemTagFolder: {
      NSFileWrapper *itemFileWrapper = parentFileWrapper.fileWrappers[[item.filePath lastPathComponent]];
      if (!itemFileWrapper) {
        NSLog(@"%s: missing file wrapper for '%@'", __func__, item.filePath);
      }
      if (![itemFileWrapper isDirectory]) {
        NSLog(@"%s: folder item '%@', but no directory wrapper '%@'", __func__, item.filePath, parentFileWrapper.filename);
        break;
      }

      updateFileWrappers(itemFileWrapper, item.children);
      break;
    }

    case PVMItemTagPackage:       // package items represent the cabal file
    case PVMItemTagFile:
    case PVMItemTagMainFile: {
      NSFileWrapper *oldFileWrapper     = item.fileWrapper;     // will change on accessing 'getUpdatedFileWrapper'
      NSFileWrapper *updatedFileWrapper = item.getUpdatedFileWrapper;
      if (updatedFileWrapper && parentFileWrapper && [parentFileWrapper isDirectory]) {

        if (![updatedFileWrapper isRegularFile]) {
          NSLog(@"%s: file item '%@', but directory wrapper '%@'", __func__, item.filePath, parentFileWrapper.filename);
          break;
        }

          // Relace the old by the updated wrapper. (The old one may not actually exist and we don't save empty files
          // created by touching.)
        if (!oldFileWrapper && [updatedFileWrapper regularFileContents].length == 0) break;
        if (oldFileWrapper) [parentFileWrapper removeFileWrapper:oldFileWrapper];
        [parentFileWrapper addFileWrapper:updatedFileWrapper];
      }
      break;
    }

    default:
      NSLog(@"%s: unknown item", __func__);
      break;
  }
}


#pragma mark -
#pragma mark Hierarchy

// Given the identifiers of the current item's children, compute its child items.
//
- (NSMutableArray/*<HFMProjectViewModelItem>*/ *)childrenFromDictionary:(NSDictionary *)dict
                                                        asSourceModules:(BOOL)processingSourceModules
{
  NSMutableArray *children             = [NSMutableArray array];
  NSString       *haskellFileExtension = [HFMProjectViewModel haskellFileExtension];

  for (NSString *string in dict) {

    NSString *identifier =  string;
    enum PVMItemTag tag  = ((NSDictionary *)dict[string]).count == 0 ? PVMItemTagFile : PVMItemTagFolder;

    if (processingSourceModules && tag == PVMItemTagFile)
    {
      if ([[string pathExtension] isEqualToString:haskellFileExtension])
        tag = PVMItemTagMainFile;
      else
        identifier = [string stringByAppendingPathExtension:haskellFileExtension];
    }
    [children addObject:[HFMProjectViewModelItem projectViewModelItemWithGroup:tag
                                                                    identifier:identifier
                                                                        parent:self
                                                                         model:self.model]];

  }
  return children;
}

// Compute the identifier dictionary containing the current item's children.
//
- (NSDictionary *)lookupDictionaryMightBeSources:(BOOL *)asSourceModules
{
  NSMutableArray          *path    = [NSMutableArray array];
  HFMProjectViewModelItem *current = self;

  while (current.tag == PVMItemTagFolder || current.tag == PVMItemTagFile || current.tag == PVMItemTagMainFile) {

    [path insertObject:current.identifier atIndex:0];
    current = current.parent;

  }

  *asSourceModules = self.parent.tag == PVMItemTagExecutable || self.parent.tag == PVMItemTagFileGroup;
  NSDictionary *dict = ([current.identifier isEqualToString:kDataGroupID]
                        || (self.parent.tag == PVMItemTagGroup
                            && [self.parent.identifier isEqualToString:kDataGroupID])) ? current.model.dataFiles :
                       ([current.identifier isEqualToString:kExtraSourceGroupID])      ? current.model.extraSrcFiles :
                       (*asSourceModules)                                              ? current.model.modules
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

// Return the item of the main module if it is directly or indirectly contained in the children.
//
- (HFMProjectViewModelItem *)containedMainFile
{
  for (HFMProjectViewModelItem *child in self.children) {

    if (child.tag == PVMItemTagMainFile) return child;

    HFMProjectViewModelItem *main = [child containedMainFile];
    if (main) return main;

  }
  return nil;     // not found
}

@end
