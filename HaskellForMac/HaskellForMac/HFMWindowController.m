//
//  HFMWindowController.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "Haskell-Swift.h"
#import "HFMProject.h"
#import "HFMWindowController.h"


@interface HFMWindowController ()

// Views in 'ProjectWindow.xib'
//
@property (weak) IBOutlet NSOutlineView *outlineView;
@property (weak) IBOutlet NSSplitView   *splitView;
@property (weak) IBOutlet NSView        *editorView;
@property (weak) IBOutlet NSTextField   *noEditorLabel;
@property (weak) IBOutlet NSView        *playgroundView;

// Our context controller (which we own).
//
@property ContextController *contextController;

// View controllers of the currently displayed editor and playground if any (which depends on the item selected in the
// outline view).
//
// We need to keep the view controllers alive here as we are in charge of entering the corresponding views into the
// view hierarchy and removing them again.
//
@property (nonatomic) NSViewController     *editorViewController;      // maybe nil
@property (nonatomic) PlaygroundController *playgroundController;      // maybe nil


@end


/// NIB file ids
//
NSString *const kGroupCellID = @"groupCellID";
NSString *const kCabalCellID = @"cabalCellID";


@implementation HFMWindowController


#pragma mark -
#pragma mark Initialisation

- (instancetype)init
{
  return [super initWithWindowNibName:@"ProjectWindow"];
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

    // Set delegate of the split view to be this window controller.
  self.splitView.delegate = self;

    // Expand all root items without animation.
  [NSAnimationContext beginGrouping];
  [[NSAnimationContext currentContext] setDuration:0];
  [self.outlineView expandItem:nil expandChildren:YES];
  [NSAnimationContext endGrouping];

    // We have got one context contoller for the lifetime of our window.
  self.contextController = [[ContextController alloc] initWithProject:self.document];
}


#pragma mark -
#pragma mark Notifications

- (void)refreshOutlineView
{
  [self.outlineView reloadData];

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
    switch (item.tag) {
      case PVMItemTagPackage:
          // FIXME: use the .hsproj icon once we have one
//        cell.imageView.image = ???;
        break;
      case PVMItemTagExecutable:
        cell.imageView.image = [[NSWorkspace sharedWorkspace] iconForFileType:@"public.unix-executable"];
        break;
      case PVMItemTagFile:
      case PVMItemTagMainFile:
        cell.imageView.image = [[NSWorkspace sharedWorkspace] iconForFileType:[item.identifier pathExtension]];
        break;
      case PVMItemTagFolder:
      case PVMItemTagFileGroup:
        cell.imageView.image = [[NSWorkspace sharedWorkspace] iconForFileType:(__bridge NSString *)kUTTypeFolder];
        break;
      default:
        break;
    }
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

    if (item && (item.tag == PVMItemTagPackage || item.tag == PVMItemTagFile || item.tag == PVMItemTagMainFile)) {

      NSViewController     *editorController     = nil;
      PlaygroundController *playgroundController = nil;

      [self.contextController selectItem:item returningEditor:&editorController playground:&playgroundController];
      [self configureEditor:editorController playground:playgroundController];
    }

  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView
shouldEditTableColumn:(NSTableColumn *)tableColumn
               item:(HFMProjectViewModelItem*)item
{
#pragma unused(outlineView, tableColumn)
  return (item.tag == PVMItemTagFile);
}


#pragma mark NSOutlineView context menu target-action methods

- (IBAction)newFile:(NSMenuItem *)sender
{
#pragma unused(sender)
  HFMProjectViewModelItem *clickedItem = [self.outlineView itemAtRow:[self.outlineView clickedRow]];
  HFMProjectViewModelItem *parentItem  = (clickedItem.tag == PVMItemTagFile || clickedItem.tag == PVMItemTagMainFile)
                                         ? [self.outlineView parentForItem:clickedItem]
                                         : clickedItem;
  NSUInteger               itemIndex   = (parentItem == clickedItem) ? 0 : [parentItem index] + 1;

    // Add a new source file to the view model and if successful...
  if ([parentItem newHaskellSourceAtIndex:itemIndex]) {

      // Update the UI.
    [self.outlineView beginUpdates];
    [self.outlineView insertItemsAtIndexes:[NSIndexSet indexSetWithIndex:itemIndex]
                                  inParent:parentItem
                             withAnimation:NSTableViewAnimationSlideDown];
    [self.outlineView endUpdates];

      // Mark document as edited.
    [self.document updateChangeCount:NSChangeDone];

  }
}

- (IBAction)delete:(NSMenuItem *)sender
{
#pragma unused(sender)

  NSInteger row = [self.outlineView clickedRow];
  if (row < 0) return;    // didn't click on an item

  HFMProjectViewModelItem *item      = [self.outlineView itemAtRow:row];
  NSUInteger               itemIndex = [item index];

    // Set up confirmation alert.
  NSAlert *alert = [[NSAlert alloc] init];
//  alert.messageText = [NSString stringWithFormat:@"Do you really want to move the %@ '%@' to the Trash?",
  alert.messageText = [NSString stringWithFormat:@"Do you really want to remove the %@ '%@'?",
                       (item.tag == PVMItemTagFile) ? @"file" : @"folder",
                       item.identifier];
//  [alert addButtonWithTitle:@"Move to Trash"];
  [alert addButtonWithTitle:@"Remove"];
  [alert addButtonWithTitle:@"Cancel"];

  if ([alert runModal] == NSAlertFirstButtonReturn) {   // Move to Trash

      // If we are deleting the currently selected entry, we need to remove it from the context.
    if ([self.outlineView selectedRow] == row) {

      [self.contextController deselectCurrentItem];
      [self configureEditor:nil playground:nil];

    }

      // Remove the file from the view model and if successful...
    if ([item remove]) {

        // FIXME: move to trash (Issue #166)

        // Update the UI.
      [self.outlineView beginUpdates];
      [self.outlineView removeItemsAtIndexes:[NSIndexSet indexSetWithIndex:itemIndex]
                                    inParent:[self.outlineView parentForItem:item]
                               withAnimation:NSTableViewAnimationSlideUp];
      [self.outlineView endUpdates];

        // Mark document as edited.
      [self.document updateChangeCount:NSChangeDone];
    }
  }
}


#pragma mark NSUserInterfaceValidations protocol methods

- (BOOL)validateUserInterfaceItem:(id <NSValidatedUserInterfaceItem>)interfaceItem
{
  SEL action = [interfaceItem action];

  if (action == @selector(newFile:)) {

    HFMProjectViewModelItem *item = [self.outlineView itemAtRow:[self.outlineView clickedRow]];
    return item.tag == PVMItemTagFolder || item.tag == PVMItemTagFileGroup || item.tag == PVMItemTagExecutable
           || item.tag == PVMItemTagFile || item.tag == PVMItemTagMainFile
           || (item.tag == PVMItemTagGroup && [item.identifier isEqualToString:kExtraSourceGroupID]);

  } else if (action == @selector(delete:)) {

    HFMProjectViewModelItem *item = [self.outlineView itemAtRow:[self.outlineView clickedRow]];
    return item.tag == PVMItemTagFolder || item.tag == PVMItemTagFileGroup || item.tag == PVMItemTagFile;

  }

  return NO;
}


#pragma mark -
#pragma mark NSSplitViewDelegate protocol methods

/* DON'T constraint the size of component views with the delegate methods, as it doesn't work properly with
 * AutoLayout.
 */

#pragma mark -
#pragma mark NSTextFieldDelegate protocol methods

- (void)controlTextDidEndEditing:(NSNotification *)notification
{
  NSTextView              *textField = [notification userInfo][@"NSFieldEditor"];
  HFMProjectViewModelItem *item = [self.outlineView itemAtRow:[self.outlineView editedRow]];

  NSString *finalName = [item renameTo:textField.textStorage.string];
//  NSString *finalName = [item renameTo:textField.stringValue];
//  if (finalName) textField.stringValue = finalName;
}


#pragma mark -
#pragma mark NSEditor protocol methods

- (BOOL)commitEditing
{
  return [self.contextController commitEditing];
}


#pragma mark -
#pragma mark Configuring the editor and playground views

/// Configure a new editor and playground controller.
///
/// First removes any old editor and/or playground, and then, installs the new ones (if the arguments are non-nil).
///
- (void)configureEditor:(NSViewController *)newEditor playground:(PlaygroundController *)newPlayground
{
    // Remove the current editor view and playground view.
  if (self.editorViewController) {

    [self.editorViewController.view removeFromSuperview];
    self.noEditorLabel.hidden = NO;

  }
  if (self.playgroundController)
    [self.playgroundController.view removeFromSuperview];

    // Enter new editor view into the view hierachy if available.
  self.editorViewController = newEditor;
  if (self.editorViewController) {

    NSView *editorContentView = self.editorViewController.view;
    editorContentView.frame = self.editorView.bounds;
    [editorContentView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
    editorContentView.translatesAutoresizingMaskIntoConstraints = YES;
    [self.editorView addSubview:editorContentView];
    self.editorView.needsLayout  = YES;
    self.editorView.needsDisplay = YES;
    self.noEditorLabel.hidden    = YES;

  }

    // Enter playground view into the view hierachy if available.
  self.playgroundController = newPlayground;
  if (self.playgroundController) {

    NSView *playgroundContentView = self.playgroundController.view;
    playgroundContentView.frame = self.playgroundView.bounds;
    [playgroundContentView setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
    playgroundContentView.translatesAutoresizingMaskIntoConstraints = YES;
    [self.playgroundView addSubview:playgroundContentView];
    self.playgroundView.needsLayout  = YES;
    self.playgroundView.needsDisplay = YES;

  }
}

@end
