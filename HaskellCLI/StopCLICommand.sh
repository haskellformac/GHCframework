#!/bin/sh

#  StopCLICommand.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 08.03.17.
#  Copyright © 2017 Manuel M T Chakravarty. All rights reserved.

pid=`ps -x | grep RunCLICommand | grep -v grep`
kill ${pid:0:6}
