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

@end


/// Outline view group ids
//
NSString *const kPackageGroupID     = @"Package";
NSString *const kDataGroupID        = @"Data";
NSString *const kExecutableGroupID  = @"Executables";
NSString *const kExtraSourceGroupID = @"Extra sources";


@implementation HFMProjectViewModelItem

@synthesize fileName = _fileName;


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

- (void)setIdentifier:(NSString *)identifier
{
  NSLog(@"%s: identifier = %@ : setting of project view model items NOT IMPLEMENTED YET", __func__, identifier);
}

- (NSString *)fileName
{
  if (!_fileName) {

    NSString *parentFileName = self.parent.fileName;

    if (self.tag == PVMItemTagPackage)
      _fileName = self.model.cabalFileName;
    else if (self.tag == PVMItemTagFile || self.tag == PVMItemTagFolder)
      _fileName = [parentFileName stringByAppendingPathComponent:self.identifier];
    else
      _fileName = @"";

  }
  return _fileName;
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

            // Data files group: multiple files in a folder hierarchy
          _theChildren = [self childrenFromDictionary:self.model.dataFiles];
          
        } else
          _theChildren = [NSMutableArray array];

        break;

      case PVMItemTagExecutable:

        _theChildren = [NSMutableArray arrayWithObject:[HFMProjectViewModelItem
                                                        projectViewModelItemWithGroup:PVMItemTagFile
                                                                           identifier:self.model.modulePath
                                                                               parent:self
                                                                                model:self.model]];

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
  return _theChildren;
}


#pragma mark -
#pragma mark Hierarchy

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

- (NSDictionary *)lookupDictionary
{
  NSMutableArray          *path    = [NSMutableArray array];
  HFMProjectViewModelItem *current = self;

  while (current.tag == PVMItemTagFolder || current.tag == PVMItemTagFile) {

    [path insertObject:current.identifier atIndex:0];
    current = current.parent;

  }

  NSDictionary *dict = ([current.identifier isEqualToString:kExtraSourceGroupID]) ? current.model.extraSrcFiles :
                       ([current.identifier isEqualToString:kDataGroupID])        ? current.model.dataFiles
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
