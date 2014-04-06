//
//  HFMHeaderEditorController.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/03/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMHeaderEditorController.h"
#import "HFMProjectViewModel.h"


@interface HFMHeaderEditorController ()

// Content views of the header editor
//
@property (weak) IBOutlet NSPathControl *pathControl;
@property (weak) IBOutlet NSTextField   *nameView;
@property (weak) IBOutlet NSTextField   *versionView;

// Object controller of the header editor
//
@property (strong) IBOutlet NSObjectController *objectController;

// Our view model and its URL.
//
@property (weak, readonly, nonatomic) HFMProjectViewModel *projectViewModel;
@property (copy)                      NSURL               *projectURLDuringInit;    // only used during set up

@end


@implementation HFMHeaderEditorController

#pragma mark Initialisation

- (instancetype)initWithNibName:(NSString *)nibName
                         bundle:(NSBundle *)nibBundle
               projectViewModel:(HFMProjectViewModel *)projectModel
                     projectURL:(NSURL *)url
{
  self = [self initWithNibName:nibName bundle:nibBundle];
  _projectViewModel     = projectModel;
  _projectURLDuringInit = url;      // can't assign to 'self.pathControl.URL' yet as 'IBOutlets' are still outstanding
  return self;
}

- (void)awakeFromNib
{
    // Initialize the path control
  self.pathControl.URL      = self.projectURLDuringInit;
  self.projectURLDuringInit = nil;
}


#pragma mark -
#pragma mark Setters and getters

- (NSURL *)URL
{
  return self.pathControl.URL;
}

- (void)setURL:(NSURL *)url
{
  self.pathControl.URL = url;
}


#pragma mark -
#pragma mark Action methods

/* Using bindings instead

// This is the action method for all text fields in the header editor.
//
- (IBAction)textFieldDidEndEditing:(NSTextField *)sender
{
  NSLog(@"editing result: %@, %d", sender.stringValue, sender == self.nameView);
}
*/

@end
