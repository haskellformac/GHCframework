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
///     file system. We leave that to the file wrappers to figure out.)
///
@property NSFileWrapper *dirtyFileWrapper;   // maybe nil

@end


/// Outline view group ids
//
NSString *const kPackageGroupID     = @"Project information";
NSString *const kExecutableGroupID  = @"Programs";
NSString *const kExtraSourceGroupID = @"Non-Haskell sources";
NSString *const kDataGroupID        = @"Supporting files";


@implementation HFMProjectViewModelItem

@synthesize fileWrapper = _fileWrapper;


#pragma mark Initialisation

+ (instancetype)projectViewModelItemWithGroup:(PVMItemTag)tag
                                   identifier:(NSString *)identifier
                                   playground:(ProjectViewModelPlayground*)playground
                                       parent:(HFMProjectViewModelItem *)parent
                                        model:(HFMProjectViewModel *)model

{
  return [[HFMProjectViewModelItem alloc] initWithGroup:tag
                                             identifier:identifier
                                             playground:playground
                                                 parent:parent
                                                  model:model];
}

- (instancetype)initWithGroup:(PVMItemTag)tag
                   identifier:(NSString *)identifier
                   playground:(ProjectViewModelPlayground*)playground
                       parent:(HFMProjectViewModelItem *)parent
                        model:(HFMProjectViewModel *)model
{
  self = [super init];
  if (self) {

    _tag         = tag;
    _identifier  = identifier;
    _playground  = playground;
    _parent      = parent;
    _model       = model;
    _theChildren = nil;

    switch (tag) {
      case PVMItemTagGroup:
        if([identifier isEqualToString:kPackageGroupID])
          _tip = @"Haskell project package";
        else if ([identifier isEqualToString:kDataGroupID])
          _tip = @"Data files included in this project";
        else if ([identifier isEqualToString:kExecutableGroupID])
          _tip = @"Executable products produced by this project";
        else if ([identifier isEqualToString:kExtraSourceGroupID])
          _tip = @"Source code in languages other than Haskell (including mark up languages)";
        break;
      case PVMItemTagPackage:
        _tip = @"Haskell project package";
        break;
      case PVMItemTagExecutable:
        _tip = @"The executable program generated from these Haskell modules";
        break;
      case PVMItemTagFile:
        _tip = [[identifier pathExtension] isEqualToString:HFMProjectViewModel.haskellFileExtension]
               ? @"Haskell module"
               : @"Supporting file";
        break;
      case PVMItemTagMainFile: {
        _tip = @"Main Haskell module of this program";
        break;
      }
      case PVMItemTagFolder:
      case PVMItemTagFileGroup:
        _tip = @"Collection of Haskell modules";
        break;
      default:
        break;
    }
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
        _fileWrapper = self.model.fileWrapper.fileWrappers[self.model.cabalFileName];
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
                                                                             playground:nil
                                                                                 parent:self
                                                                                  model:self.model]];

        } else if ([self.identifier isEqualToString:kExecutableGroupID]) {

            // Executable group: 1 child: executable section
          _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                          projectViewModelItemWithGroup:PVMItemTagExecutable
                                                                             identifier:self.model.executableName
                                                                             playground:nil
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
                                                            playground:nil
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
                                                                             playground:nil
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

/// Returns the URL from which this item was loaded if any.
///
/// WARNING: Use this sparringly. Only use this for file operations and for Quicklook previews, which operate on the
///          underlying file; otherwise, always use the data from the file wrapper instead. Before accessing the file
///          system representation, ensure the project was saved (i.e., isn't dirty)!
///
- (NSURL *)URL
{
  return [self.model.documentURL URLByAppendingPathComponent:self.filePath];
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
  if (!data)                // if `self.fileWrapper == nil`
    data = [NSData data];
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


#pragma mark -
#pragma mark Edits

- (void)touchFileWrapper
{
  if (!self.fileWrapper) {
    self.dirtyFileWrapper                   = [[NSFileWrapper alloc] initRegularFileWithContents:[NSData data]];
    self.dirtyFileWrapper.preferredFilename = self.identifier;
  }
}

- (BOOL)copyFileAtURL:(NSURL *)url toIndex:(NSUInteger)index error:(NSError *__autoreleasing *)error
{
  if ((self.tag != PVMItemTagGroup)
      && self.tag != PVMItemTagExecutable
      && self.tag != PVMItemTagFileGroup
      && self.tag != PVMItemTagFolder) return NO;
  if (self.children.count < index) return NO;

    // Attempt to copy the file. (We attempt to remove any file at the target's name first; if there is a name clash,
    // the caller will have made sure that overwritting the original file is ok.)
  NSFileManager *fileManager = [NSFileManager defaultManager];
  NSString      *identifier  = [url lastPathComponent];
  NSURL         *destination = [[self URL] URLByAppendingPathComponent:identifier];
  if ([fileManager fileExistsAtPath:[destination path]] &&
      ![fileManager trashItemAtURL:destination resultingItemURL:nil error:error])
    return NO;
  if (![fileManager copyItemAtURL:url toURL:destination error:error])
    return NO;

    // Update the current item's file wrapper (to pick up the new file).
  if (![self.fileWrapper readFromURL:[self URL] options:0 error:error])
    return NO;

    // Grab the file wrapper of the new item.
  NSFileWrapper *newItemFileWrapper = self.fileWrapper.fileWrappers[identifier];
  if (!newItemFileWrapper) {

    *error = [NSError errorWithDomain:@"HFM Internal Error" code:-1 userInfo:nil];
    return NO;

  }

  HFMProjectViewModelItem *newChild= [HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagFile
                                                                                 identifier:identifier
                                                                                 playground:nil
                                                                                     parent:self
                                                                                      model:self.model];
  [self.theChildren insertObject:newChild atIndex:index];

  return [self.model writeCabalFileWithError:error];
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
  NSString *identifier = [[Swift swift_nextName:@"NewSource" usedNames:usedNames]
                          stringByAppendingPathExtension:[HFMProjectViewModel haskellFileExtension]];
  ProjectViewModelPlayground *newPlayground = [[ProjectViewModelPlayground alloc] initWithIdentifier:identifier
                                                                                               model:self.model];
  HFMProjectViewModelItem    *newChild      = [HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagFile
                                                                                          identifier:identifier
                                                                                          playground:newPlayground
                                                                                              parent:self
                                                                                               model:self.model];
  [self.theChildren insertObject:newChild atIndex:index];
  newChild.string = @"";      // This marks the items as dirty.
  return YES;
}

/*
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
 */

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
  if (self.fileWrapper)
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

    // Make sure the new identifier is unique (can't let the file wrappers do this as `self.fileWrapper` may be `nil`.
  NSMutableArray *usedNames = [NSMutableArray array];
  NSMutableArray *children  = [self.parent.theChildren mutableCopy];
  [children removeObject:self];
  for (HFMProjectViewModelItem *child in children)
    [usedNames addObject:[child.identifier stringByDeletingPathExtension]];

  NSString *uniqueIdentifier = ([newIdentifier pathExtension].length == 0)
                               ? [Swift swift_nextName:newIdentifier usedNames:usedNames]
                               : [[Swift swift_nextName:[newIdentifier stringByDeletingPathExtension] usedNames:usedNames]
                                  stringByAppendingPathExtension:[newIdentifier pathExtension]];

    // NB: If we try to set the `preferredFileName` without removing the file wrapper from its parent first, we trigger
    //     a fast enumeration exception — although the docs suggest that the file wrapper class would handle that
    //     automatically. Moreover, we disambiguate the name ourselves, because duplicate during adding also leads to
    //     an exception.
  if (self.fileWrapper) {
    [parentFileWrapper removeFileWrapper:self.fileWrapper];
    self.fileWrapper.preferredFilename = uniqueIdentifier;
    [parentFileWrapper addFileWrapper:self.fileWrapper];
  }
  if (self.playground)
    [self.playground renameToTrackModule:uniqueIdentifier parent:parentFileWrapper];

  self.identifier = uniqueIdentifier;
  return ([newIdentifier isEqualToString:uniqueIdentifier] ? nil : uniqueIdentifier);
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
      if (!parentFileWrapper || ![parentFileWrapper isDirectory]) break;

        // Update the file associated with the current item.
      NSFileWrapper *oldFileWrapper     = item.fileWrapper;     // will change on accessing 'getUpdatedFileWrapper'
      NSFileWrapper *updatedFileWrapper = item.getUpdatedFileWrapper;
      if (updatedFileWrapper) {

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

        // Update the playground, if any is associated with the current item.
      if (item.playground) {

        NSFileWrapper *playgroundFileWrapper    = item.playground.fileWrapper;
        NSFileWrapper *oldPlaygroundFileWrapper = parentFileWrapper.fileWrappers[playgroundFileWrapper.preferredFilename];

        if (![oldPlaygroundFileWrapper isEqual:playgroundFileWrapper]) {
          if (oldPlaygroundFileWrapper) [parentFileWrapper removeFileWrapper:oldPlaygroundFileWrapper];
          [parentFileWrapper addFileWrapper:playgroundFileWrapper];
        }
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

    NSString *identifier                   =  string;
    enum PVMItemTag tag                    = ((NSDictionary *)dict[string]).count == 0 ? PVMItemTagFile : PVMItemTagFolder;
    ProjectViewModelPlayground *playground = nil;

    if (processingSourceModules && tag == PVMItemTagFile)
    {
      if ([[string pathExtension] isEqualToString:haskellFileExtension])    // only the main file has an extension in the model
        tag = PVMItemTagMainFile;
      else
        identifier = [string stringByAppendingPathExtension:haskellFileExtension];

      NSString      *playgroundFilename    = [ProjectViewModelPlayground implicitPlaygroundFilename:identifier];
      NSFileWrapper *playgroundFileWrapper = self.fileWrapper.fileWrappers[playgroundFilename];
      if (playgroundFileWrapper)
        playground = [[ProjectViewModelPlayground alloc] initWithFileWrapper:playgroundFileWrapper model:self.model];
      else
        playground = [[ProjectViewModelPlayground alloc] initWithIdentifier:identifier model:self.model];
    }
    [children addObject:[HFMProjectViewModelItem projectViewModelItemWithGroup:tag
                                                                    identifier:identifier
                                                                    playground:playground
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


#pragma mark -
#pragma mark QLPreviewItem protocol methods

- (NSURL *)previewItemURL
{
  return [self URL];
}

@end
