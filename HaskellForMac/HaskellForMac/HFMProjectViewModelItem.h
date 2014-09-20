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
  PVMItemTagGroup,            // Toplevel group in the outline view
  PVMItemTagPackage,          // Identifies the Cabal packages itself
  PVMItemTagExecutable,       // Executable secion of the Cabal package
  PVMItemTagFileGroup,        // Logical collection of files (wildcard name or source directory or data directory)
  PVMItemTagFolder,           // File system directory
  PVMItemTagFile,             // Concrete file that is part of the package
  PVMItemTagMainFile,         // Concrete file that is part of the package and the main module of an executable
};

/// Outline view group ids
//
extern NSString *const kPackageGroupID;
extern NSString *const kDataGroupID;
extern NSString *const kExecutableGroupID;
extern NSString *const kExtraSourceGroupID;


@interface HFMProjectViewModelItem : NSObject

/// Determines which flavour of project view model item this is.
///
@property (readonly) PVMItemTag tag;

/// Identifier that is used to display the item.
///
@property (nonatomic) NSString *identifier;

/// *Optional* file wrapper of the file object backing the item.
///
/// This property is computed lazily. This property is 'nil' for non file-related items.
///
/// The reference is weak as the file wrapper object is owned by either its parent directory wrapper or, if it is the
/// document file wrapper, by the view model object.
///
@property (nonatomic, readonly, weak) NSFileWrapper *fileWrapper;     // maybe nil

/// Does this item have unsaved changes?
///
@property (readonly, getter=isDirty) BOOL dirty;

/// The text contained in 'fileWrapper', if not 'dirty'; otherwise, the updated text. When this property is set, the
/// item becomes dirty (and an updated file wrapper is transparently created, which may be accessed via
/// 'getUpdatedFileWrapper').
///
/// These properties are KVO compliant. Both properties refer to the same data, but present it in different formats.
///
@property NSString           *string;                 // 'nil' unless 'fileWrapper' is wrapping a regular file
@property NSAttributedString *attributedString;       // 'nil' unless 'fileWrapper' is wrapping a regular file


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

/// Is the item representing an empty folder or a folder containing nothing but empty folders?
///
- (BOOL)isEmptyFolder;

/// The index of the current item in the children array of its parent.
///
/// `NSNotFound` if it has no parent.
///
- (NSUInteger)index;

/// Returns an array containing all child items of the current item.
///
- (NSArray/*<HFMProjectModelItem>*/ *)children;

/// Returns the item's file name relative to the document root, or the empty string if the item is not associated with
/// a file.
///
- (NSString *)fileName;

/// Returns an updated file wrapper in case this item is 'dirty'; otherwise, returns 'nil'. If the item is dirty, the
/// 'fileWrapper' property will also be updated to refer to the updated wrapper.
///
/// After this method was invoked, the item is not dirty (until the next change) — i.e., discarding the returned
/// file wrapper will discard any changes since the last save.
///
- (NSFileWrapper *)getUpdatedFileWrapper;


#pragma mark -
#pragma mark Edits

/// Create a new empty file wrapper for regular file items whose file wrapper is `nil`. (These are files that are
/// listed in the Cabal file, but do not exist in the file system.)
///
/// NB: If no text is entered, we do not create an empty file in the file system on save.
///
- (void)touchFileWrapper;

/// Add a child item associated with the given file wrapper.
///
/// The current item must be represented by a directory.
///
- (void)addChild:(HFMProjectViewModelItem *)child fileWrapper:(NSFileWrapper *)childFileWrapper;

/// Move the current item from the view model and the file wrapper structure (deleteing it in the next save).
///
/// @Returns: Was the operation sucessful?
///
- (BOOL)remove;

/// Remove the current given item from the list of children of the current item.
///
/// @Returns: Was the operation sucessful?
///
- (BOOL)removeChild:(HFMProjectViewModelItem *)childItem;


#pragma mark -
#pragma mark File wrapper update

/// For any dirty view model item, replace the corresponding file wrapper by its updated file wrapper.
///
/// The provided file wrapper is that of the folder directly containing the given items. The functions recursively
/// traverse the item trees.
///
void updateFileWrappers(NSFileWrapper *parentFileWrapper, NSArray *items);
void updateFileWrapper(NSFileWrapper *parentFileWrapper, HFMProjectViewModelItem *item);

@end
