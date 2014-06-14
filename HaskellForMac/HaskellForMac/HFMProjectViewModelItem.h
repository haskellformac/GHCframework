//
//  HFMProjectViewModelItem.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 13/06/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This class provides a view model representation of the individual items that are displayed in the project's outline
//  view. Following the requirements of 'NSOutlineView', each view model item is only used to represent a single item
//  in the outline view.

#import <Foundation/Foundation.h>
#import "HFMProjectViewModel.h"


/// Tag determining which of several flavours of project view model items we have.
//
typedef NS_ENUM(NSUInteger, PVMItemTag) {
  PVMItemTagGroup,
  PVMItemTagPackage,
  PVMItemTagExecutable,
  PVMItemTagFolder,
  PVMItemTagFile,
};

/// Outline view group ids
//
extern NSString *const kPackageGroupID;
extern NSString *const kDataGroupID;
extern NSString *const kExecutableGroupID;
extern NSString *const kExtraSourceGroupID;


@interface HFMProjectViewModelItem : NSObject

/// Determines which flavour of project view model item this is.
//
@property (readonly) PVMItemTag tag;

/// Identifier that is used to display the item.
//
@property (nonatomic) NSString *identifier;

/// File name identifying the location of the file object backing the item *relative* to the root document.
///
/// This property is computed lazily.
//
@property (nonatomic, readonly) NSString *fileName;


#pragma mark -
#pragma mark Initialisation

/// Create a new item of a given flavour with an identifier that is used to display the item.
//
+ (instancetype)projectViewModelItemWithGroup:(PVMItemTag)tag
                                   identifier:(NSString *)identifier
                                       parent:(HFMProjectViewModelItem *)parent
                                        model:(HFMProjectViewModel *)model;

/// Initialise a new item of a given flavour with an identifier that is used to display the item.
//
- (instancetype)initWithGroup:(PVMItemTag)tag
                   identifier:(NSString *)identifier
                       parent:(HFMProjectViewModelItem *)parent
                        model:(HFMProjectViewModel *)model;


#pragma mark -
#pragma mark Queries

/// Returns an array containing all child items of the current item.
//
- (NSArray/*<HFMProjectModelItem>*/ *)children;

@end
