//
//  ArrayExtensions.swift
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 22/09/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

import Foundation


/// Swift version of Haskell's `List.all`
///
func all<S: SequenceType>(sequence: S, condition: S.Generator.Element -> Bool) -> Bool {
    return reduce(sequence, true){ a, b in a && condition(b) }
}
