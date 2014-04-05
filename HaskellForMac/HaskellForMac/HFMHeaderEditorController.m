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
@property (weak) IBOutlet NSTextField   *nameView;
@property (weak) IBOutlet NSTextField   *versionView;
@property (weak) IBOutlet NSPathControl *pathControl;

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
    // Initialize views with values from the Cabal file
  self.nameView.stringValue    = self.projectViewModel.name;
  self.versionView.stringValue = self.projectViewModel.version;
  self.pathControl.URL         = self.projectURLDuringInit;
  self.projectURLDuringInit    = nil;
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

@end
