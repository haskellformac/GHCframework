//
//  MACAddress.m
//  HaskellForMac
//
//  Created by Manuel M T Chakravarty on 8/10/2014.
//  Copyright (c) 2014 Manuel M T Chakravarty. All rights reserved.
//

#import <IOKit/IOKitLib.h>
#import "MACAddress.h"

// This code is from https://developer.apple.com/library/ios/releasenotes/General/ValidateAppStoreReceipt/Chapters/ValidateLocally.html#//apple_ref/doc/uid/TP40010573-CH1-SW14

CFDataRef copy_mac_address(void)
{
  kern_return_t             kernResult;
  mach_port_t               master_port;
  CFMutableDictionaryRef    matchingDict;
  io_iterator_t             iterator;
  io_object_t               service;
  CFDataRef                 macAddress = nil;

  kernResult = IOMasterPort(MACH_PORT_NULL, &master_port);
  if (kernResult != KERN_SUCCESS) {
    printf("IOMasterPort returned %d\n", kernResult);
    return nil;
  }

  matchingDict = IOBSDNameMatching(master_port, 0, "en0");
  if (!matchingDict) {
    printf("IOBSDNameMatching returned empty dictionary\n");
    return nil;
  }

  kernResult = IOServiceGetMatchingServices(master_port, matchingDict, &iterator);
  if (kernResult != KERN_SUCCESS) {
    printf("IOServiceGetMatchingServices returned %d\n", kernResult);
    return nil;
  }

  while((service = IOIteratorNext(iterator)) != 0) {
    io_object_t parentService;

    kernResult = IORegistryEntryGetParentEntry(service, kIOServicePlane,
                                               &parentService);
    if (kernResult == KERN_SUCCESS) {
      if (macAddress) CFRelease(macAddress);

      macAddress = (CFDataRef) IORegistryEntryCreateCFProperty(parentService,
                                                               CFSTR("IOMACAddress"), kCFAllocatorDefault, 0);
      IOObjectRelease(parentService);
    } else {
      printf("IORegistryEntryGetParentEntry returned %d\n", kernResult);
    }

    IOObjectRelease(service);
  }
  IOObjectRelease(iterator);

  return macAddress;
}