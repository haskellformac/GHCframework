//
//  main.swift
//  StandardizePath
//
//  Created by Manuel M T Chakravarty on 14/11/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//
//  Uses the 'NSString' operation 'stringByResolvingSymlinksInPath' on its first argument to produce a canonical path.

import Foundation

print(NSURL(fileURLWithPath: Process.arguments[1]).resolvingSymlinksInPath()!.path!)
