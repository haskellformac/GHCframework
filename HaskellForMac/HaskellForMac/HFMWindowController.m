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


/// Admissible window configuartions
///
typedef NS_ENUM(NSInteger, WindowConfiguration) {
  WindowConfigurationAllVisible,    // All panes are visible (includes playground iff currently edited file is Haskell)
  WindowConfigurationNoPlayground,  // No playground: only the source view and editor
  WindowConfigurationOnlyEditor,    // Only editor (neither source view nor playground)
  WindowConfigurationNoSourceView,  // Editor and playground (no source view)
  WindowConfigurationOnlyPlayground // Only playground (neither source view nor editor)
};


@interface HFMWindowController ()

// Views in 'ProjectWindow.xib'
//
@property (weak) IBOutlet NSScrollView    *outlineScrollView;
@property (weak) IBOutlet NSOutlineView   *outlineView;
@property (weak) IBOutlet StyledSplitView *splitView;
@property (weak) IBOutlet NSView          *editorView;
@property (weak) IBOutlet NSTextField     *noEditorLabel;
@property (weak) IBOutlet NSView          *playgroundView;

// Our cloud and local context controller (which we own).
//
@property CloudController   *cloudController;
@property ContextController *contextController;

// View controllers of the currently displayed editor and playground if any (which depends on the item selected in the
// outline view).
//
// We need to keep the view controllers alive here as we are in charge of entering the corresponding views into the
// view hierarchy and removing them again.
//
@property (nonatomic) NSViewController     *editorViewController;      // maybe nil
@property (nonatomic) PlaygroundController *playgroundController;      // maybe nil

/// Active window configuration.
///
@property (nonatomic) WindowConfiguration windowConfiguration;

@end


/// NIB file ids
//
NSString *const kGroupCellID = @"groupCellID";
NSString *const kCabalCellID = @"cabalCellID";

void windowElementVisibility(WindowConfiguration  windowConfiguration,
                             BOOL                *isSourceViewVisible,
                             BOOL                *isEditorViewVisible,
                             BOOL                *isPlaygroundVisible);


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

    // Window configuration.
    // FIXME: This needs to be made persistent.
  _windowConfiguration = WindowConfigurationAllVisible;

    // We have got one cloud controller and one local context contoller for the lifetime of our window.
  self.cloudController   = [[CloudController alloc] initWithProject:self.document
                                              authenticationRequest:^(AuthenticationFlavour auth) {
#pragma unused(auth)
                                                NSAlert *alert = [[NSAlert alloc] init];
                                                alert.messageText = @"To use Cloudcelerate, you need to create an account.";
                                                alert.informativeText = @"The new account is tied to this copy of Haskell for Mac. By creating an account you agree to the Terms and Conditions.";
                                                [alert addButtonWithTitle:@"Create account"];
                                                [alert addButtonWithTitle:@"Do not create account"];
                                                return (BOOL)([alert runModal] == NSAlertFirstButtonReturn);
                                              }];
  self.contextController = [[ContextController alloc] initWithProject:self.document];

    // FIXME: This is to early. We need to wait until the window elements have restored there state. Do we get a notification for that???
  if ([self.splitView isSubviewCollapsed:self.outlineScrollView])
    _windowConfiguration = WindowConfigurationNoSourceView;
  else
    _windowConfiguration = WindowConfigurationAllVisible;
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
#pragma mark Menu actions

  // NB: Needs to be enabled in `-valideUserInterfaceItems:`.
- (void)newCloudAccount:(id)sender
{
#pragma unused(sender)
  [self.cloudController ping];
}

  // NB: Needs to be enabled in `-valideUserInterfaceItems:`.
- (void)runProjectInCloud:(id)sender
{
#pragma unused(sender)
  [self.cloudController run];
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

    NSTableCellView *cell      = [outlineView makeViewWithIdentifier:kGroupCellID owner:self];
    cell.textField.stringValue = [item.identifier uppercaseString];
    cell.textField.toolTip     = item.tip;
    cell.imageView.toolTip     = item.tip;
    return cell;


  } else {

    NSTableCellView *cell      = [outlineView makeViewWithIdentifier:kCabalCellID owner:self];
    cell.textField.stringValue = item.identifier;
    cell.textField.toolTip     = item.tip;
    cell.imageView.toolTip     = item.tip;
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

- (void)outlineViewSelectionIsChanging:(NSNotification *)notification
{
#pragma unused(notification)
  [self.contextController commitEditing];
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

        // Load the newly selected module right away.
      [self.contextController loadContextModule];
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


#pragma mark -
#pragma mark NSOutlineView context menu target-action methods

#pragma mark File menu target-action methods

- (IBAction)openInEditor:(NSMenuItem *)sender
{
#pragma unused(sender)
  NSInteger row = [self.outlineView clickedRow] == -1 ? [self.outlineView selectedRow]
                                                      : [self.outlineView clickedRow];
  if (row < 0) return;    // no item clicked or selected

  HFMProjectViewModelItem *clickedItem = [self.outlineView itemAtRow:row];
  HFMProject              *project     = (HFMProject*)self.document;

  [[NSWorkspace sharedWorkspace] openURL:[project.fileURL URLByAppendingPathComponent:clickedItem.filePath]];
    // FIXME: if editor set in defaults, invoke the set editor with the following message:
//  [[NSWorkspace sharedWorkspace] openFile:[project.fileURL URLByAppendingPathComponent:clickedItem.filePath].path
//                          withApplication:@"???"];
}

- (IBAction)showInFinder:(NSMenuItem *)sender
{
#pragma unused(sender)
  NSInteger row = [self.outlineView clickedRow] == -1 ? [self.outlineView selectedRow]
                                                      : [self.outlineView clickedRow];
  if (row < 0) return;    // no item clicked or selected

  HFMProjectViewModelItem *clickedItem = [self.outlineView itemAtRow:row];
  HFMProject              *project     = (HFMProject*)self.document;

  [[NSWorkspace sharedWorkspace]
   activateFileViewerSelectingURLs:@[[project.fileURL URLByAppendingPathComponent:clickedItem.filePath]]];
}

- (IBAction)newFile:(NSMenuItem *)sender
{
#pragma unused(sender)
  NSInteger row = [self.outlineView clickedRow] == -1 ? [self.outlineView selectedRow]
                                                      : [self.outlineView clickedRow];
  if (row < 0) return;    // no item clicked or selected

  HFMProjectViewModelItem *clickedItem = [self.outlineView itemAtRow:row];
  HFMProjectViewModelItem *parentItem  = (clickedItem.tag == PVMItemTagFile || clickedItem.tag == PVMItemTagMainFile)
                                         ? [self.outlineView parentForItem:clickedItem]
                                         : clickedItem;
  NSUInteger               itemIndex   = (parentItem == clickedItem) ? 0 : [clickedItem index] + 1;
  HFMProject              *project     = (HFMProject*)self.document;

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

      // Select and enter editing mode for the newly added item.
    HFMProjectViewModelItem *newItem = [project outlineView:self.outlineView child:(NSInteger)itemIndex ofItem:parentItem];
    [self performSelector:@selector(fileEdit:) withObject:newItem afterDelay:0.3];
      // NB: After returning from the current method, the selected row gets deselected, interrupting editing. So, we
      //     delay editing. It does seem like a hack, though. Is there any better way to achieve this?
  }
}

- (void)fileEdit:(HFMProjectViewModelItem*)newItem
{
  [self.outlineView editColumn:0 row:[self.outlineView rowForItem:newItem] withEvent:nil select:YES];
}

- (IBAction)rename:(NSMenuItem *)sender {
#pragma unused(sender)

  NSInteger row = [self.outlineView clickedRow] == -1 ? [self.outlineView selectedRow]
                                                      : [self.outlineView clickedRow];
  if (row < 0) return;    // no item clicked or selected

  HFMProjectViewModelItem *clickedItem = [self.outlineView itemAtRow:row];
  [self.outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:(NSUInteger)row] byExtendingSelection:NO];
  [self fileEdit:clickedItem];
}

- (IBAction)delete:(NSMenuItem *)sender
{
#pragma unused(sender)

  NSInteger row = [self.outlineView clickedRow] == -1 ? [self.outlineView selectedRow]
                                                      : [self.outlineView clickedRow];
  if (row < 0) return;    // no item clicked or selected

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

#pragma mark View menu target-action methods

- (IBAction)toggleNavigatorView:(id)sender
{
#pragma unused(sender)

  [self.splitView animateSetSubview:self.outlineScrollView toCollapsed:!self.outlineScrollView.hidden completionHandler:^{
      // We cannot show the source view without the editor view.
    if (!self.outlineScrollView.hidden && self.editorView.hidden) [self toggleEditorView:sender];
  }];
}

- (IBAction)toggleEditorView:(id)sender
{
#pragma unused(sender)

    // We cannot hide the editor unless the playground is visible.
  if (self.playgroundView.hidden && !self.editorView.hidden)
    [self.splitView animateSetSubview:self.playgroundView toCollapsed:NO completionHandler:^{
      if (!self.editorView.hidden)
        [self toggleEditorView:sender];
    }];
  else
    [self.splitView animateSetSubview:self.editorView toCollapsed:!self.editorView.hidden completionHandler:^{
        // We cannot hide the editor view without hiding the source view.
      if (self.editorView.hidden && !self.outlineScrollView.hidden) [self toggleNavigatorView:sender];
    }];
}

- (IBAction)togglePlaygroundView:(id)sender
{
#pragma unused(sender)

    // We cannot hide the playground unless the editor is visible.
  if (self.editorView.hidden && !self.playgroundView.hidden)
    [self.splitView animateSetSubview:self.editorView toCollapsed:NO completionHandler:^{
      if (!self.playgroundView.hidden)
        [self togglePlaygroundView:sender];
    }];
  else
    [self.splitView animateSetSubview:self.playgroundView toCollapsed:!self.playgroundView.hidden completionHandler:nil];
}

- (IBAction)moveViewLeft:(id)sender
{
#pragma unused(sender)

  if (!self.playgroundView.hidden && self.editorView.hidden)
    [self.splitView animateSetSubview:self.editorView toCollapsed:NO completionHandler:nil];         // reveal editor
  else if (!self.playgroundView.hidden && !self.editorView.hidden && self.outlineScrollView.hidden)
    [self.splitView animateSetSubview:self.outlineScrollView toCollapsed:NO completionHandler:nil];  // reveal navigator
  else if (!self.playgroundView.hidden && !self.editorView.hidden && !self.outlineScrollView.hidden)
    [self.splitView animateSetSubview:self.playgroundView toCollapsed:YES completionHandler:nil];    // hide playground
}

- (IBAction)moveViewRight:(id)sender
{
#pragma unused(sender)

  if (self.playgroundView.hidden)
    [self.splitView animateSetSubview:self.playgroundView toCollapsed:NO completionHandler:nil];     // reveal playground
  else if (!self.playgroundView.hidden && !self.outlineScrollView.hidden)
    [self.splitView animateSetSubview:self.outlineScrollView toCollapsed:YES completionHandler:nil]; // hide navigator
  else if (!self.playgroundView.hidden && !self.editorView.hidden)
    [self.splitView animateSetSubview:self.editorView toCollapsed:YES completionHandler:nil];        // hide editor
}

#pragma mark Navigate menu target-action methods (forwarded)

- (void)jumpToNextIssue:(id)sender
{
  [self.contextController jumpToNextIssue:sender];
}

- (void)jumpToPreviousIssue:(id)sender
{
  [self.contextController jumpToPreviousIssue:sender];
}

#pragma mark NSMenuValidation and NSUserInterfaceValidations protocol methods

  // We set the appropriate menu titles in this method and defer to '-validateUserInterfaceItem:' for the actual
  // validation.
- (BOOL)validateMenuItem:(NSMenuItem *)menuItem
{
  SEL action = [menuItem action];

  if (action == @selector(toggleNavigatorView:)) {

    menuItem.title = self.outlineScrollView.hidden ? @"Show Project Navigator" : @"Hide Project Navigator";

  } else if (action == @selector(toggleEditorView:)) {

    menuItem.title = self.editorView.hidden ? @"Show Editor" : @"Hide Editor";
    
  } else if (action == @selector(togglePlaygroundView:)) {

    menuItem.title = self.playgroundView.hidden ? @"Show Playground" : @"Hide Playground";
    
  }
  return [self validateUserInterfaceItem:menuItem];
}


  // NB: At the moment, we only support individual selections (and not groups of selections). This will have to change
  //     at some point. Once, we supported selected groups, and the user right-clicks for the context menu, we need to
  //     check whether the clicked is part of the selected group. If so, the action ought to apply to the entire group.
- (BOOL)validateUserInterfaceItem:(id <NSValidatedUserInterfaceItem>)interfaceItem
{
  SEL       action = [interfaceItem action];
  NSInteger row    = [self.outlineView clickedRow] == -1 ? [self.outlineView selectedRow]
                                                         : [self.outlineView clickedRow];

  if (action == @selector(openInEditor:) || action == @selector(showInFinder:)) {

    HFMProjectViewModelItem *item = [self.outlineView itemAtRow:row];
    return item.tag == PVMItemTagFolder || item.tag == PVMItemTagFileGroup || item.tag == PVMItemTagFile
           || item.tag == PVMItemTagMainFile;

  } else if (action == @selector(newFile:)) {

    HFMProjectViewModelItem *item = [self.outlineView itemAtRow:row];
    return item.tag == PVMItemTagFolder || item.tag == PVMItemTagFileGroup || item.tag == PVMItemTagExecutable
           || item.tag == PVMItemTagFile || item.tag == PVMItemTagMainFile
           || (item.tag == PVMItemTagGroup && [item.identifier isEqualToString:kExtraSourceGroupID]);

  } else if (action == @selector(rename:)) {

    HFMProjectViewModelItem *item = [self.outlineView itemAtRow:row];
    return item.tag == PVMItemTagFolder || item.tag == PVMItemTagFileGroup || item.tag == PVMItemTagFile;

  } else if (action == @selector(delete:)) {

    HFMProjectViewModelItem *item = [self.outlineView itemAtRow:row];
    return item.tag == PVMItemTagFolder || item.tag == PVMItemTagFileGroup || item.tag == PVMItemTagFile;

  } else if (action == @selector(toggleNavigatorView:)) {

    return YES;

  } else if (action == @selector(toggleEditorView:)) {

    return YES;
    
  } else if (action == @selector(togglePlaygroundView:)) {

    return YES;

  } else if (action == @selector(moveViewLeft:)) {

    return self.outlineScrollView.hidden || self.editorView.hidden || !self.playgroundView.hidden;
    
  } else if (action == @selector(moveViewRight:)) {

    return !self.outlineScrollView.hidden || !self.editorView.hidden || self.playgroundView.hidden;

  } else if (action == @selector(newCloudAccount:)) {

    return ![self.cloudController accountStatus];

  } else if (action == @selector(runProjectInCloud:)) {

    return YES; // FIXME: Should only be YES if there are no errors etc and the cloud is not offline etc.

  } else if (action == @selector(jumpToNextIssue:) || action == @selector(jumpToPreviousIssue:)) {

      // When a diagnostics popup is presented, the responder chain goes from the popup to the main window; we
      // redirect to the editor
    return [self.contextController validateUserInterfaceItem:interfaceItem];

  }

  return NO;
}


#pragma mark -
#pragma mark NSSplitViewDelegate protocol methods

/* DON'T constraint the size of component views with the delegate methods, as it doesn't work properly with
 * AutoLayout. In particular, don't use any of 'splitView:constrainMinCoordinate:ofSubviewAt:',
 * 'splitView:constrainMaxCoordinate:ofSubviewAt:', 'splitView:resizeSubviewsWithOldSize:', and
 * 'splitView:shouldAdjustSizeOfSubview:'.
 */

- (BOOL)splitView:(NSSplitView *)splitView canCollapseSubview:(NSView *)subview
{
#pragma unused(splitView)

  return (subview == self.editorView) ? NO : YES;
}


#pragma mark -
#pragma mark NSTextFieldDelegate protocol methods

- (BOOL)control:(NSControl *)control isValidObject:(NSString*)string
{
#pragma unused(control)

    // Accept Haskell module names with a '.hs' suffix (for modules) and plain Haskell module names for folders.
  NSString *extension = [string pathExtension];
  NSString *name      = [string stringByDeletingPathExtension];
  return ([Swift swift_isValidModuleName:name] && [extension isEqualToString:[HFMProjectViewModel haskellFileExtension]])
         || [Swift swift_isValidModuleName:string];
}

  // This is used when the editing of a text field of the source view ends.
- (void)controlTextDidEndEditing:(NSNotification *)notification
{
  NSText                  *text      = notification.userInfo[@"NSFieldEditor"];
  NSTextField             *textField = notification.object;
  HFMProjectViewModelItem *item      = [self.outlineView itemAtRow:[self.outlineView selectedRow]];
  NSString                *oldName   = item.identifier;
  NSString                *newName   = text.string;

    // Add a Haskell file extension to file names if not present yet.
  if ((item.tag == PVMItemTagFile || item.tag == PVMItemTagMainFile)
      && ![[newName pathExtension] isEqualToString:[HFMProjectViewModel haskellFileExtension]]) {
    newName = [newName stringByAppendingPathExtension:[HFMProjectViewModel haskellFileExtension]];
    textField.stringValue = newName;
  }
  NSString *finalName = [item renameTo:newName];
  if (finalName) textField.stringValue = finalName;

    // Mark document as edited.
  if (![textField.stringValue isEqualToString:oldName])
    [self.document updateChangeCount:NSChangeDone];

    // Make sure any change of the string in this method is reflected in the UI.
  [self.outlineView reloadItem:item];
}


#pragma mark -
#pragma mark NSEditor protocol methods

- (BOOL)commitEditing
{
  return [self.contextController commitEditing];
}


#pragma mark -
#pragma mark Configuring the window configuration

void windowElementVisibility(WindowConfiguration  windowConfiguration,
                             BOOL                *isSourceViewVisible,
                             BOOL                *isEditorViewVisible,
                             BOOL                *isPlaygroundVisible)
{
  switch (windowConfiguration) {
    case WindowConfigurationAllVisible:
      *isSourceViewVisible = YES;
      *isEditorViewVisible = YES;
      *isPlaygroundVisible = YES;
      break;

    case WindowConfigurationNoPlayground:
      *isSourceViewVisible = YES;
      *isEditorViewVisible = YES;
      *isPlaygroundVisible = NO;
      break;

    case WindowConfigurationOnlyEditor:
      *isSourceViewVisible = NO;
      *isEditorViewVisible = YES;
      *isPlaygroundVisible = NO;
      break;

    case WindowConfigurationNoSourceView:
      *isSourceViewVisible = NO;
      *isEditorViewVisible = YES;
      *isPlaygroundVisible = YES;
      break;

    case WindowConfigurationOnlyPlayground:
      *isSourceViewVisible = NO;
      *isEditorViewVisible = NO;
      *isPlaygroundVisible = YES;
      break;

    default:
      NSLog(@"%s: unknown configuration", __func__);
      *isSourceViewVisible = YES;
      *isEditorViewVisible = YES;
      *isPlaygroundVisible = YES;
      break;
  }
}

- (void)setWindowConfiguration:(WindowConfiguration)newWindowConfiguration
{
  BOOL /*isSourceViewVisibleBefore,*/ isSourceViewVisibleAfter;
  BOOL /*isEditorViewVisibleBefore,*/ isEditorViewVisibleAfter;
  BOOL /*isPlaygroundVisibleBefore,*/ isPlaygroundVisibleAfter;

//  windowElementVisibility(self.windowConfiguration,
//                          &isSourceViewVisibleBefore, &isEditorViewVisibleBefore, &isPlaygroundVisibleBefore);
// We would need the above information for animation, to determine whether there was a change.
  windowElementVisibility(newWindowConfiguration,
                          &isSourceViewVisibleAfter, &isEditorViewVisibleAfter, &isPlaygroundVisibleAfter);


//    [self.outlineScrollView setHidden:!isSourceViewVisibleAfter];

  [self.splitView animateSetSubview:self.outlineScrollView toCollapsed:!isSourceViewVisibleAfter completionHandler:nil];
#if 0
//  if (self.outlineScrollView.hidden && isSourceViewVisibleAfter) {
  if (self.outlineScrollView.hidden) {
    CGRect frame = self.outlineScrollView.frame;
    [NSAnimationContext currentContext].duration = 0.25;
//    [self.outlineScrollView setFrameSize:(CGSize){ .width = 1, .height = frame.size.height }];
//    [[self.splitView animator] setPosition:1 ofDividerAtIndex:0];
    NSLayoutConstraint *constraint = [NSLayoutConstraint constraintWithItem:self.outlineScrollView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:50];

    [self.outlineScrollView setHidden:NO];

    NSArray *oldConstraints = self.outlineScrollView.constraints;
    [self.outlineScrollView removeConstraints:oldConstraints];
    [self.outlineScrollView addConstraint:constraint];
    /*
    dispatch_async(dispatch_get_main_queue(), ^{
     */
      [NSAnimationContext beginGrouping];
    [NSAnimationContext currentContext].completionHandler = ^{ [self.outlineScrollView removeConstraint:constraint]; [self.outlineScrollView addConstraints:oldConstraints];
      [self.splitView adjustSubviews];
    };
    [[constraint animator] setConstant:frame.size.width];
//      [[self.outlineScrollView animator] setFrameSize:frame.size];
//      [[self.splitView animator] setPosition:frame.size.width ofDividerAtIndex:0];
      [NSAnimationContext endGrouping];
    /*
    });
     */

  } else
    [self.outlineScrollView setHidden:YES];
//  [self.outlineScrollView setHidden:!isSourceViewVisibleAfter];
#endif


  [self.editorView setHidden:!isEditorViewVisibleAfter];
  [self.playgroundView setHidden:!isPlaygroundVisibleAfter];
//  NSLayoutPriority editorHoldingPriority     = [self.splitView holdingPriorityForSubviewAtIndex:1];
//  NSLayoutPriority playgroundHoldingPriority = [self.splitView holdingPriorityForSubviewAtIndex:2];
//  [self.splitView setHoldingPriority:NSLayoutPriorityDefaultLow forSubviewAtIndex:1];
//  [self.splitView setHoldingPriority:NSLayoutPriorityDefaultLow forSubviewAtIndex:2];
//  [self.splitView adjustSubviews];
//  [self.splitView setHoldingPriority:editorHoldingPriority forSubviewAtIndex:1];
//  [self.splitView setHoldingPriority:playgroundHoldingPriority forSubviewAtIndex:2];
    //    [self.splitView setPosition:0 ofDividerAtIndex:0];
  _windowConfiguration = newWindowConfiguration;
}


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
