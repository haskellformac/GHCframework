//
//  HFMProjectViewModel.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/02/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "CabalKit/CabalKit.h"
#import "HFMProjectViewModel.h"
#import "HFMProjectViewModelItem.h"


@interface HFMProjectViewModel ()

// The Cabal package describing the project. 'CBLPackage' objects are immutable — i.e., whenever package data changes,
// this property will be updated.
//
// This is our interface to the Haskell-side data model.
//
@property CBLPackage *package;

@end


// Function prototypes

void updateFileWrappers(NSFileWrapper *projectFileWrapper, NSArray *items);
void updateFileWrapper(NSFileWrapper *projectFileWrapper, HFMProjectViewModelItem *item);
void replaceFileWrapper(NSFileWrapper *projectFileWrapper,
                             NSString *fname,
                        NSFileWrapper *oldFileWrapper,
                        NSFileWrapper *updatedFileWrapper);

NSDictionary *stringsToDictTree(NSArray      *strings);
NSArray      *dictTreeToStrings(NSDictionary *dicts);


@implementation HFMProjectViewModel

@synthesize identifier = _identifier;


#pragma mark -
#pragma mark Informative class methods

static NSString *cabalFileExtension = @"cabal";
+ (NSString *)cabalFileExtension
{
  return cabalFileExtension;
}

static NSString *haskellFileExtension = @"hs";
+ (NSString *)haskellFileExtension
{
  return haskellFileExtension;
}


#pragma mark -
#pragma mark Initialisation

+ (instancetype)projectViewModelWithProjectFileWrapper:(NSFileWrapper *)projectFileWrapper
                                      cabalFileWrapper:(NSFileWrapper *)cabalFileWrapper
{
  return [[HFMProjectViewModel alloc] initWithProjectFileWrapper:projectFileWrapper
                                                cabalFileWrapper:cabalFileWrapper];
}

- (instancetype)init
{
  NSLog(@"%s: cannot initialise the view model without a name for the Cabal file", __func__);
  return nil;
}

- (instancetype)initWithProjectFileWrapper:(NSFileWrapper *)projectFileWrapper
                          cabalFileWrapper:(NSFileWrapper *)cabalFileWrapper
{
  self = [super init];
  if (self) {

    _fileWrapper = projectFileWrapper;

      // If no Cabal file present, create a new, empty Cabal package whose name is derived from the package name and add
      // it to the file wrapper for the project.
    if (!cabalFileWrapper) {

        // Create a new empty package description.
      _package = [CBLPackage package];

        // Wrap it into a new file wrapper.
      NSString *fname = [[[projectFileWrapper preferredFilename] stringByDeletingPathExtension]
                         stringByAppendingPathExtension:cabalFileExtension];
      NSData   *data  = [[self.package string] dataUsingEncoding:NSUTF8StringEncoding];
      [projectFileWrapper addRegularFileWithContents:data preferredFilename:fname];
      cabalFileWrapper = [projectFileWrapper fileWrappers][fname];
      
    } else {
    
      NSString *cabalContents = [[NSString alloc] initWithData:[cabalFileWrapper regularFileContents]
                                                      encoding:NSUTF8StringEncoding];

      _package = [CBLPackage packageWithString:cabalContents];
      if (!_package) return nil;    // Bail out if we cannot load the Cabal file

    }
    _cabalFileName = cabalFileWrapper.preferredFilename;

      // Set up the group items
    [self initGroupItems];
  }
  return self;
}

- (void)initGroupItems
{
    // We initialise the immutable project groups (which forms the root set of the project view model items). Child
    // items are created on demand.
    //
    // IMPORTANT: The order here must match that in 'PVMGroupIndex'.
  _groupItems = @[[HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagGroup
                                                              identifier:kPackageGroupID
                                                                  parent:nil
                                                                   model:self],
                  [HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagGroup
                                                              identifier:kDataGroupID
                                                                  parent:nil
                                                                   model:self],
                  [HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagGroup
                                                              identifier:kExecutableGroupID
                                                                  parent:nil
                                                                   model:self],
                  [HFMProjectViewModelItem projectViewModelItemWithGroup:PVMItemTagGroup
                                                              identifier:kExtraSourceGroupID
                                                                  parent:nil
                                                                   model:self],
                  ];
}

- (void)createProjectForURL:(NSURL *)url
{
  NSString *path         = [url path];
  NSString *projname     = [path lastPathComponent];
  NSString *name         = [projname stringByDeletingPathExtension];
  NSString *mainName     = [name stringByAppendingPathExtension:[HFMProjectViewModel haskellFileExtension]];
  NSString *oldCabalName = [self.cabalFileName lastPathComponent];
  NSString *newCabalName = [name stringByAppendingPathExtension:[HFMProjectViewModel cabalFileExtension]];

    // The URL is where we want to save the project and we need to change the cached Cabal filename, too.
  self.fileWrapper.preferredFilename = projname;
  _cabalFileName                     = newCabalName;

    // Configure basic Cabal fields.
  self.name           = name;
  self.version        = @"1.0";
  self.license        = @"AllRightsReserved";
  self.executableName = name;
  self.sourceDir      = nil;
  self.modulePath     = mainName;

    // Create a new file wrapper for the new Cabal file.
  [self.fileWrapper removeFileWrapper:[self.fileWrapper fileWrappers][oldCabalName]];
  [self.fileWrapper addRegularFileWithContents:[[self.package string] dataUsingEncoding:NSUTF8StringEncoding]
                             preferredFilename:newCabalName];

    // Create main Haskell file
  [self.fileWrapper addRegularFileWithContents:[NSData data] preferredFilename:mainName];

    // Write new project out and recreate the group items.
  NSError *error;
  if (![self.fileWrapper writeToURL:url
                            options:NSFileWrapperWritingWithNameUpdating
                originalContentsURL:nil
                              error:&error])
    NSLog(@"%s: couldn't create project files: %@", __func__, error);
  [self initGroupItems];
}


#pragma mark -
#pragma mark Setters and getters

- (NSString *)identifier
{
  if (!_identifier) _identifier = [self.package identifier];
  return _identifier;
}

- (void)setName:(NSString *)name
{
  self.package = [CBLPackage package:self.package withName:name];
  NSLog(@"Update project name");
}

- (NSString *)name
{
  return self.package.name;
}

- (void)setVersion:(NSString *)version
{
  self.package = [CBLPackage package:self.package withVersion:version];
  NSLog(@"Update project version");
}

- (NSString *)version
{
  return self.package.version;
}

- (void)setCategory:(NSString *)category
{
  self.package = [CBLPackage package:self.package withCategory:category];
  NSLog(@"Update project category");
}

- (NSString *)category
{
  return self.package.category;
}

- (void)setSynopsis:(NSString *)synopsis
{
  self.package = [CBLPackage package:self.package withSynopsis:synopsis];
  NSLog(@"Update project synopsis");
}

- (NSString *)synopsis
{
  return self.package.synopsis;
}

- (void)setFullDescription:(NSString *)fullDescription
{
  self.package = [CBLPackage package:self.package withFullDescription:fullDescription];
  NSLog(@"Update project description");
}

- (NSString *)fullDescription
{
  return self.package.fullDescription;
}

- (void)setAuthor:(NSString *)author
{
  self.package = [CBLPackage package:self.package withFullDescription:author];
  NSLog(@"Update project author");
}

- (NSString *)author
{
  return self.package.author;
}

- (void)setMaintainer:(NSString *)maintainer
{
  self.package = [CBLPackage package:self.package withMaintainer:maintainer];
  NSLog(@"Update project maintainer");
}

- (NSString *)maintainer
{
  return self.package.maintainer;
}

- (void)setCopyright:(NSString *)copyright
{
  self.package = [CBLPackage package:self.package withCopyright:copyright];
  NSLog(@"Update project copyright");
}

- (NSString *)copyright
{
  return self.package.copyright;
}

- (void)setLicense:(NSString *)license
{
  self.package = [CBLPackage package:self.package withLicense:license];
  NSLog(@"Update project license");
}

- (NSString *)license
{
  return self.package.license;
}

- (void)setHomepage:(NSString *)homepage
{
  self.package = [CBLPackage package:self.package withHomepage:homepage];
  NSLog(@"Update project homepage");
}

- (NSString *)homepage
{
  return self.package.homepage;
}

- (void)setPkgUrl:(NSString *)pkgUrl
{
  self.package = [CBLPackage package:self.package withPkgUrl:pkgUrl];
  NSLog(@"Update project package URL");
}

- (NSString *)pkgUrl
{
  return self.package.pkgUrl;
}

- (void)setBugReports:(NSString *)bugReports
{
  self.package = [CBLPackage package:self.package withBugReports:bugReports];
  NSLog(@"Update project bug reports");
}

- (NSString *)bugReports
{
  return self.package.bugReports;
}

- (void)setDataDir:(NSString *)dataDir
{
  self.package = [CBLPackage package:self.package withDataDir:dataDir];
  NSLog(@"Update project data directory");
}

- (NSString *)dataDir
{
  return self.package.dataDir;
}

- (void)setDataFiles:(NSDictionary *)dataFiles
{
  self.package = [CBLPackage package:self.package withDataFiles:dictTreeToStrings(dataFiles)];
  NSLog(@"Update project data files");
}

- (NSDictionary *)dataFiles
{
  return stringsToDictTree(self.package.dataFiles);
}

- (void)setExtraSrcFiles:(NSDictionary *)extraSrcFiles
{
  self.package = [CBLPackage package:self.package withExtraSrcFiles:dictTreeToStrings(extraSrcFiles)];
  NSLog(@"Update project extra source files");
}

- (NSDictionary *)extraSrcFiles
{
  return stringsToDictTree(self.package.extraSrcFiles);
}

// executable section

- (void)setExecutableName:(NSString *)executableName
{
  self.package = [CBLPackage package:self.package withExecutableName:executableName];
  NSLog(@"Update project executable name");
}

- (NSString *)executableName
{
  return self.package.executableName;
}

- (void)setSourceDir:(NSString *)sourceDir
{
  self.package = [CBLPackage package:self.package withSourceDir:sourceDir];
  NSLog(@"Update project executable source directory");
}

- (NSString *)sourceDir
{
  return self.package.sourceDir;
}

- (void)setModulePath:(NSString *)modulePath
{
  self.package = [CBLPackage package:self.package withModulePath:modulePath];
  NSLog(@"Update project executable Main module path");
}

- (NSString *)modulePath
{
  return self.package.modulePath;
}

- (void)setOtherModules:(NSDictionary *)otherModules
{
  self.package = [CBLPackage package:self.package withOtherModules:dictTreeToStrings(otherModules)];
  NSLog(@"Update project executable other modules");
}

- (NSDictionary *)otherModules
{
  return stringsToDictTree(self.package.otherModules);
}


#pragma mark -
#pragma mark Project serialisation

- (NSFileWrapper *)fileWrapperWithError:(NSError *__autoreleasing *)outError
{
#pragma unused(outError)

    // Flush any changes in the Cabal file
  HFMProjectViewModelItem *packageGroupItem = self.groupItems[PVMItemGroupIndexPackage];
  HFMProjectViewModelItem *cabalFileItem    = packageGroupItem.children[0];
  cabalFileItem.string = [self.package string];
   // FIXME: currently we always overwrite the Cabal file, which is bad
   //   we should rather only do the above 'updateItemWithData:' IFF any data in the Cabal file changed

  NSFileWrapper *projectFileWrapper = self.fileWrapper;
  updateFileWrappers(projectFileWrapper, self.groupItems);

  return projectFileWrapper;
}

void updateFileWrappers(NSFileWrapper *projectFileWrapper, NSArray *items)
{
  for (HFMProjectViewModelItem *item in items)
    updateFileWrapper(projectFileWrapper, item);
}

void updateFileWrapper(NSFileWrapper *projectFileWrapper, HFMProjectViewModelItem *item)
{
  NSFileWrapper *oldFileWrapper     = item.fileWrapper;
  NSFileWrapper *updatedFileWrapper = item.getUpdatedFileWrapper;

    // FIXME: FOR THE MOMENT, we assume folders and groups cannot be updated (i.e., updated => is a file item).
  if (updatedFileWrapper)
    replaceFileWrapper(projectFileWrapper, item.fileName, oldFileWrapper, updatedFileWrapper);
  else
    updateFileWrappers(projectFileWrapper, item.children);
}

void replaceFileWrapper(NSFileWrapper *currentFileWrapper,
                        NSString      *fname,
                        NSFileWrapper *oldFileWrapper,
                        NSFileWrapper *updatedFileWrapper)
{
    // Find the directory file wrapper corresponding to 'fname'.
  for (NSString *component in [[fname stringByDeletingLastPathComponent] pathComponents]) {

    if (!currentFileWrapper || ![currentFileWrapper isDirectory]) break;
    currentFileWrapper = [currentFileWrapper fileWrappers][component];

  }
  if (currentFileWrapper && [currentFileWrapper isDirectory]) {

      // Relace the old by the update wrapper.
    [currentFileWrapper removeFileWrapper:oldFileWrapper];
    [currentFileWrapper addFileWrapper:updatedFileWrapper];

  } else
    NSLog(@"%s: couldn't find file wrapper for directory named %@", __func__, fname);
}


#pragma mark -
#pragma mark Data conversions

NSDictionary *stringsToDictTree(NSArray *strings)
{
  NSMutableDictionary *dict = [NSMutableDictionary dictionary];

  for (NSString *fname in strings) {

    NSArray             *components = [fname pathComponents];
    NSMutableDictionary *cursor     = dict;
    for (NSString *component in components) {

      if (!cursor[component])             // component not yet in the tree
        cursor[component] = [NSMutableDictionary dictionary];

      cursor = cursor[component];

    }
  }
  return [NSDictionary dictionaryWithDictionary:dict];    // freeze
}

NSArray *dictTreeToStrings(NSDictionary *dicts)
{
  NSMutableArray *strings = [NSMutableArray array];

  for (NSString *component in dicts) {

    NSArray *substrings = dictTreeToStrings(dicts[component]);
    if ([substrings count] == 0)
      [strings addObject:component];
    else
      for (NSString *substring in substrings)
        [strings addObject:[component stringByAppendingPathComponent:substring]];

  }
  return [NSArray arrayWithArray:strings];    // freeze
}

@end
