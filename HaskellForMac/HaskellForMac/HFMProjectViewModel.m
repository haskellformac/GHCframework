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


#pragma mark -
#pragma mark Project serialisation

- (NSString *)string
{
    // FIXME: do we still need to synchronise the view model with the model (or the view data)?
  return [self.package string];
}


@end
