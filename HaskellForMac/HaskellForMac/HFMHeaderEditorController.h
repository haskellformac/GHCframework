//
//  HFMHeaderEditorController.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/03/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This is the view controller for the package header editor (i.e., for the Cabal header data).

#import <Cocoa/Cocoa.h>
#import "HFMProjectViewModel.h"


@interface HFMHeaderEditorController : NSViewController

// The URL of the path control of the editor.
//
@property (strong, nonatomic) NSURL *URL;

// Initialise the view controller by loading its NIB file and also set the associated view model and its URL.
//
- (instancetype)initWithNibName:(NSString *)nibName
                         bundle:(NSBundle *)nibBundle
               projectViewModel:(HFMProjectViewModel *)projectModel
                     projectURL:(NSURL *)url;

@end
