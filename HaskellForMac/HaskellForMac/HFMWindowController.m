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
@property (weak)              IBOutlet NSOutlineView *outlineView;
@property (weak)              IBOutlet NSSplitView   *verticalSplitView;
@property (weak)              IBOutlet NSSplitView   *horizontalSplitView;
@property (weak)              IBOutlet NSView        *editorView;
@property (weak)              IBOutlet NSTextField   *noEditorLabel;
@property (unsafe_unretained) IBOutlet NSTextView    *replView;


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

  if (row != -1) {   // If a row is selected...

    HFMProjectViewModelItem *item = [outlineView itemAtRow:row];

    if (item && (item.tag == PVMItemTagPackage || item.tag == PVMItemTagFile))
      [self selectEditor:item];

  }
}


#pragma mark -
#pragma mark NSSplitViewDelegate protocol methods

/* DON'T constraint the size of component views with the delegate methods, as it doesn't work properly with
 * AutoLayout.
 */


#pragma mark -
#pragma mark Controlling the editor component

/// Select the editor appropriate to editing the file backing the given given view model item; the type of editor is
/// determined on the basis of the file extension.
///
/// If no suitable editor is available, remove the current editor view (if any).
//
- (void)selectEditor:(HFMProjectViewModelItem *)item
{
  HFMProject *project       = self.document;
  NSString   *fileName      = item.fileName;
  NSURL      *fileURL       = [project.fileURL URLByAppendingPathComponent:fileName];
  NSString   *fileExtension = [fileName pathExtension];
  NSError    *error;

  if (!fileName) return;
  if (![item.fileWrapper isRegularFile]) return;

    // Check that the file is still there and force reading its contents. (We'll need it in a sec.)
  if (![item.fileWrapper readFromURL:fileURL options:NSFileWrapperReadingImmediate error:&error]) {
    NSLog(@"%s: re-reading file wrapper from %@ failed: %@", __func__, fileURL, error);
    return;
  }

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
                                              projectURL:fileURL];

  } else if ([nibName isEqual:kTextEditor]) {

    self.editorViewController = [[HFMTextEditorController alloc] initWithNibName:nibName
                                                                          bundle:nil
                                                            projectViewModelItem:item
                                                                         fileURL:fileURL];
      // FIXME: TEMPORARY HACK
    item.loadString = ^(NSString *moduleText) {

      NSString           *result   = [[self.haskellSession loadModuleFromString:moduleText] stringByAppendingString:@"\n\n"];
      NSFont             *menlo13  = [NSFont fontWithName:@"Menlo-Regular" size:13];
      NSAttributedString *attrText = [[NSAttributedString alloc] initWithString:result
                                                                     attributes:@{ NSFontAttributeName : menlo13 }];
      [self.replView.textStorage appendAttributedString:attrText];
      [self.replView scrollRangeToVisible:NSMakeRange([self.replView.textStorage length], 0)];

    };

  }
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
