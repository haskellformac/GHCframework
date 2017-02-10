#!/bin/sh

#  RunHaskellTerminal.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 07.02.17.
#  Copyright © 2017 Manuel M T Chakravarty. All rights reserved.
#
#  Run an executable of a Haskell for Mac project in a Terminal.app window. Ensure that the window isn't automatically
#  closed when execution finishes.
#
#  * The first argument is the main Haskell module to execute. All remaining arguments are arguments to the Haskell code.
#  * The current working directory must be the Haskell project. The HASKELL_DATA_FOLDER environment variable must
#    contain the current data folder if any is set; otherwise, the variable must not be set.

CLITOOLS=BIN_PATH

executable=$1
shift

command="cd ${PWD}; ${CLITOOLS}/runhaskell $@"
tab=`osascript -e 'tell app "Terminal" to do script "'"${command}"'"'`

osascript -e 'tell app "Terminal" to set title displays window size of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set title displays file name of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set title displays device name of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set title displays shell path of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set custom title of '"${tab}"' to "Haskell for Mac: '"${executable}"'"'
