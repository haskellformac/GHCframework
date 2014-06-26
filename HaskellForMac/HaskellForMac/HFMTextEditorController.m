//
//  HFMTextEditorController.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/03/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import "HFMTextEditorController.h"


@interface HFMTextEditorController ()

/// Content views of the header editor
//
@property (weak)              IBOutlet NSPathControl *pathControl;
@property (unsafe_unretained) IBOutlet NSTextView    *textView;

/// File wrapper identifying the edited file.
//
@property (readonly) NSFileWrapper *fileWrapper;

// Text file URL.
//
@property (copy) NSURL *fileURLDuringInit;    // only used during set up

@end


@implementation HFMTextEditorController

#pragma mark Initialisation

- (instancetype)initWithNibName:(NSString *)nibName
                         bundle:(NSBundle *)nibBundle
                    fileWrapper:(NSFileWrapper *)fileWrapper
                        fileURL:(NSURL *)fileURL
{
  self = [self initWithNibName:nibName bundle:nibBundle];
  _fileWrapper       = fileWrapper;
  _fileURLDuringInit = fileURL;
  return self;

}

- (void)awakeFromNib
{
  NSError *error;

    // Initialize the path control
  self.pathControl.URL = _fileURLDuringInit;
  _fileURLDuringInit   = nil;

    // FIXME: quick and dirty setting of the contents — implement properly handle file data through the model layer
  NSString *contents = [NSString stringWithContentsOfURL:self.pathControl.URL encoding:NSUTF8StringEncoding error:&error];
  if (!contents)
    NSLog(@"%s: error loading file %@: %@", __func__, self.pathControl.URL, error);
  else {

    NSFont             *menlo13      = [NSFont fontWithName:@"Menlo-Regular" size:13];
    NSAttributedString *attrContents = [[NSAttributedString alloc] initWithString:contents
                                                                       attributes:@{ NSFontAttributeName : menlo13 }];
    [self.textView.textStorage appendAttributedString:attrContents];

  }
}

@end
