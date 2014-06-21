//
//  HFMTextEditorController.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/03/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMTextEditorController.h"


@interface HFMTextEditorController ()

// Content views of the header editor
//
@property (weak)              IBOutlet NSPathControl *pathControl;
@property (unsafe_unretained) IBOutlet NSTextView    *textView;

// Text file URL.
//
@property (copy) NSURL *fileURLDuringInit;    // only used during set up

@end


@implementation HFMTextEditorController

#pragma mark Initialisation

- (instancetype)initWithNibName:(NSString *)nibName
                         bundle:(NSBundle *)nibBundle
                        fileURL:(NSURL *)url
{
  self = [self initWithNibName:nibName bundle:nibBundle];
  _fileURLDuringInit = url;      // can't assign to 'self.pathControl.URL' yet as 'IBOutlets' are not yet initialised
  return self;

}

- (void)awakeFromNib
{
    // Initialize the path control
  self.pathControl.URL      = self.fileURLDuringInit;
  self.fileURLDuringInit = nil;
}



@end
