//
//  HFMTextEditorController.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 31/03/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  This is the view controller for the text editor.

#import <Cocoa/Cocoa.h>


@interface HFMTextEditorController : NSViewController

// Initialise the view controller by loading its NIB file and also set the associated file URL.
//
- (instancetype)initWithNibName:(NSString *)nibName
                         bundle:(NSBundle *)nibBundle
                    fileWrapper:(NSFileWrapper *)fileWrapper
                        fileURL:(NSURL *)fileURL;

@end
