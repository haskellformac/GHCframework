//
//  Geometry.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 21/01/2015.
//  Copyright (c) 2015 Manuel M T Chakravarty. All rights reserved.
//

import Foundation

func clampExtent(extent: CGFloat, min: CGFloat, max: CGFloat) -> CGFloat {
  if extent < min { return min }
  else if extent > max { return max }
  else { return extent }
}

