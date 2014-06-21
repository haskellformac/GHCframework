//
//  HFMWindowController.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMWindowController.h"
#import "HFMProject.h"
#import "HFMHeaderEditorController.h"
#import "HFMTextEditorController.h"
#import "HFMHaskellSession.h"


@interface HFMWindowController ()

// Views in 'ProjectWindow.xib'
//
@property (weak) IBOutlet NSOutlineView *outlineView;
@property (weak) IBOutlet NSSplitView   *verticalSplitView;
@property (weak) IBOutlet NSSplitView   *horizontalSplitView;
@property (weak) IBOutlet NSView        *editorView;
@property (weak) IBOutlet NSTextField   *noEditorLabel;

/// View controller of the currently displayed editor (which depends on the item selected in the outline view).
///
/// The corresponding views are specified in separate '.xib' files. We need to keep the view controller alive here.
//
@property (nonatomic) NSViewController *editorViewController;

/// The GHC session associated with this window.
//
@property (nonatomic, readonly) HFMHaskellSession *haskellSession;

/// A dictionary associating file extensions with the editor used to edit files of that type. Editors are identified
/// be the name of their NIB file.
//
@property (nonatomic, readonly) NSDictionary *editors;

@end


/// Editor NIB file names
//
NSString *const kPackageHeaderEditor = @"PackageHeaderEditor";
NSString *const kTextEditor          = @"TextEditor";

/// NIB file ids
//
NSString *const kGroupCellID = @"groupCellID";
NSString *const kCabalCellID = @"cabalCellID";


@implementation HFMWindowController


#pragma mark -
#pragma mark Initialisation

- (instancetype)init
{
  self = [super initWithWindowNibName:@"ProjectWindow"];
  if (self) {

    _haskellSession = [HFMHaskellSession haskellSessionStart];
    NSLog(@"WindowController: session start");

    _editors = @{@"cabal": kPackageHeaderEditor,
                 @"hs":    kTextEditor,
                 @"txt":   kTextEditor,
                 @"md":    kTextEditor};

  }
  return self;
}

- (void)windowDidLoad
{
  [super windowDidLoad];

    // Initialise the size and data for the project outline view. The delegate is this window controller and data source
    // is the document project.
  [self.outlineView sizeLastColumnToFit];
  self.outlineView.delegate   = self;
  self.outlineView.dataSource = self.document;
  [self.outlineView reloadData];

    // Set delegate of the split views is this window controller.
  self.verticalSplitView.delegate   = self;
  self.horizontalSplitView.delegate = self;

    // Expand all root items without animation.
  [NSAnimationContext beginGrouping];
  [[NSAnimationContext currentContext] setDuration:0];
  [self.outlineView expandItem:nil expandChildren:YES];
  [NSAnimationContext endGrouping];
}


#pragma mark -
#pragma mark NSOutlineViewDelegate protocol methods

- (BOOL)outlineView:(NSOutlineView *)outlineView isGroupItem:(HFMProjectViewModelItem *)item
{
#pragma unused(outlineView)

  return item.tag == PVMItemTagGroup;
}

- (NSTableCellView *)outlineView:(NSOutlineView *)outlineView
              viewForTableColumn:(NSTableColumn *)tableColumn
                            item:(HFMProjectViewModelItem *)item
{
#pragma unused(tableColumn)     // there is only one column

    // Do we need a group cell or a cabal cell item?
  if (item.tag == PVMItemTagGroup) {

    NSTableCellView *cell = [outlineView makeViewWithIdentifier:kGroupCellID owner:self];
    cell.textField.stringValue = [item.identifier uppercaseString];
    return cell;


  } else {

    NSTableCellView *cell = [outlineView makeViewWithIdentifier:kCabalCellID owner:self];
    cell.textField.stringValue = item.identifier;
    return cell;

  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView shouldSelectItem:(HFMProjectViewModelItem *)item
{
#pragma unused(outlineView)

  return item.tag != PVMItemTagGroup;
}

- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  NSOutlineView *outlineView = [notification object];
  NSInteger      row         = [outlineView selectedRow];
  HFMProject    *document    = self.document;

  if (row != -1) {   // If a row is selected...

    HFMProjectViewModelItem *item = [outlineView itemAtRow:row];

    if (item && (item.tag == PVMItemTagPackage || item.tag == PVMItemTagFile))
        // FIXME: once we open a folder (and not .cabal file), drop the 'URLByDeletingLastPathComponent'!!!
      [self selectEditor:[[document.fileURL URLByDeletingLastPathComponent] URLByAppendingPathComponent:item.fileName]];

  }
}


#pragma mark -
#pragma mark NSSplitViewDelegate protocol methods

/* DON'T constraint the size of component views with the delegate methods, as it doesn't work properly with
 * AutoLayout.
 */


#pragma mark -
#pragma mark Controlling the editor component

- (void)selectEditor:(NSURL *)file
{
  NSString *fileExtension = [file pathExtension];

  if (!file) return;

    // Remove the current editor view.
  if (self.editorViewController) {

    [[self.editorViewController view] removeFromSuperview];
    self.noEditorLabel.hidden = NO;

  }
    // Select suitable editor.
  NSString *nibName = [self.editors objectForKey:fileExtension];
  if (!nibName)
    return;

    // Load the new view by way of the matching view controller.
  if ([nibName isEqual:kPackageHeaderEditor]) {

    HFMProject *project = self.document;

    self.editorViewController =
      [[HFMHeaderEditorController alloc] initWithNibName:nibName
                                                  bundle:nil
                                        projectViewModel:project.projectModel
                                              projectURL:project.fileURL];

  } else if ([nibName isEqual:kTextEditor])
    self.editorViewController = [[HFMTextEditorController alloc] initWithNibName:nibName bundle:nil fileURL:file];
  if (!self.editorView) {

    NSLog(@"%s: cannot load editor nib %@", __func__, nibName);
    return;

  }

    // Enter editor view into the view hierachy.
  NSView *view = [self.editorViewController view];
  view.frame = self.editorView.bounds;
  [view setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
  view.translatesAutoresizingMaskIntoConstraints = YES;
  [self.editorView addSubview:view];
  self.editorView.needsLayout  = YES;
  self.editorView.needsDisplay = YES;

  self.noEditorLabel.hidden = YES;

}

@end
