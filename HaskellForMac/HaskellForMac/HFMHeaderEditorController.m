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
@property (weak, atomic) IBOutlet NSTextField *nameView;
@property (weak, atomic) IBOutlet NSTextField *versionView;

// Our view model
//
@property (weak, readonly, nonatomic) HFMProjectViewModel *projectViewModel;

@end


@implementation HFMHeaderEditorController

- (instancetype)initWithNibName:(NSString *)nibName
                         bundle:(NSBundle *)nibBundle
               projectViewModel:(HFMProjectViewModel *)projectModel
{
  self = [self initWithNibName:nibName bundle:nibBundle];
  _projectViewModel = projectModel;
  return self;
}

- (void)awakeFromNib
{
    // Initialize views with values from the Cabal file
  self.nameView.stringValue    = self.projectViewModel.name;
  self.versionView.stringValue = self.projectViewModel.version;
}

@end
