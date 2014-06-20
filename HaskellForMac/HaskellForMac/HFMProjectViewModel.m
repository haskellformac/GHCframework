//
//  HFMProjectViewModel.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/02/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProjectViewModel.h"
#import "HFMProjectViewModelItem.h"
#import "CBLPackage_objc.h"


@interface HFMProjectViewModel ()

// The Cabal package describing the project. 'CBLPackage' objects are immutable — i.e., whenever package data changes,
// this property will be updated.
//
// This is our interface to the Haskell-side data model.
//
@property CBLPackage *package;

@end


// Function prototypes
NSDictionary *stringsToDictTree(NSArray      *strings);
NSArray      *dictTreeToStrings(NSDictionary *dicts);


@implementation HFMProjectViewModel

@synthesize identifier = _identifier;


#pragma mark -
#pragma mark Initialisation

+ (instancetype)projectViewModelWithCabalFileName:(NSString *)cabalFileName
{
  return [[HFMProjectViewModel alloc] initWithCabalFileName:cabalFileName];
}

+ (instancetype)projectViewModelWithCabalFileName:(NSString *)cabalFileName string:(NSString *)string
{
  return [[HFMProjectViewModel alloc] initWithCabalFileName:cabalFileName string:string];
}

- (instancetype)init
{
  return [self initWithCabalFileName:@"Untitled.cabal"];
}

- (instancetype)initWithCabalFileName:(NSString *)cabalFileName
{
  return [self initWithCabalFileName:cabalFileName string:nil];
}

- (instancetype)initWithCabalFileName:(NSString *)cabalFileName string:(NSString *)string
{
  self = [super init];
  if (self) {

    _package       = (string) ? [CBLPackage packageWithString:string] : [CBLPackage package];
    _cabalFileName = cabalFileName;

      // We initialise the immutable project groups (which forms the root set of the project view model items). Child
      // items are created on demand.
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
  return self;
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

- (void)setModulePath:(NSString *)modulePath
{
  self.package = [CBLPackage package:self.package withModulePath:modulePath];
  NSLog(@"Update project executable Main module path");
}

- (NSString *)modulePath
{
  return self.package.modulePath;
}


#pragma mark -
#pragma mark Project serialisation

- (NSString *)string
{
    // FIXME: do we still need to synchronise the view model with the model (or the view data)?
  return [self.package string];
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
