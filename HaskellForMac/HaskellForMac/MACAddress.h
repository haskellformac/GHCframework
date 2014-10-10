//
//  MACAddress.h
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#ifndef HaskellForMac_MACAddress_h
#define HaskellForMac_MACAddress_h

#import <Foundation/Foundation.h>

// Returns a CFData object, containing the computer's GUID (MAC address).
//
CFDataRef copy_mac_address(void);

#endif
