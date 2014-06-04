//
//  HFMProjectViewModel.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 7/02/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMProjectViewModel.h"
#import "CBLPackage_objc.h"


@interface HFMProjectViewModel ()

// The Cabal package describing the project. 'CBLPackage' objects are immutable — i.e., whenever package data changes,
// this property will be updated.
//
// This is our interface to the Haskell-side data model.
//
@property CBLPackage *package;

@end


@implementation HFMProjectViewModel

@synthesize identifier = _identifier;


#pragma mark -
#pragma mark Initialisation

+ (instancetype)projectViewModel
{
  return [[HFMProjectViewModel alloc] init];
}

+ (instancetype)projectViewModelWithString:(NSString *)string
{
  return [[HFMProjectViewModel alloc] initWithString:string];
}

- (instancetype)init
{
  self = [super init];
  if (self)
    _package = [CBLPackage package];
  return self;
}

- (instancetype)initWithString:(NSString *)string
{
  self = [super init];
  if (self)
    _package = [CBLPackage packageWithString:string];
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



#pragma mark -
#pragma mark Project serialisation

- (NSString *)string
{
    // FIXME: do we still need to synchronise the view model with the model (or the view data)?
  return [self.package string];
}


@end
