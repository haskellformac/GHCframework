#!/bin/sh

#  RunHaskellTerminal.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 07.02.17.
#  Copyright © [2017..2018] Manuel M T Chakravarty. All rights reserved.
#
#  Run an executable of a Haskell for Mac project in a Terminal.app window. Ensure that the window isn't automatically
#  closed when execution finishes.
#
#  * The first argument is the name of the project executable that is being executed.
#  * The second argument is the working directory (which ought to be the Haskell project).
#  * The third argument is the data folder, or "none" if no data folder has been set.
#  * The fourth argument is the main Haskell module to execute, *relative* to the working directory. All remaining
#    arguments are arguments to the Haskell code.

CLITOOLS=BIN_PATH

executable=$1
shift
wd=$1
shift
data_folder=$1
shift

if [ ${data_folder} = "none" ];
then
  command="cd ${wd}; clear; ${CLITOOLS}/runhaskell -- -fdefer-type-errors -fno-ghci-sandbox $@"
else
  command="cd ${wd}; clear; env HASKELL_DATA_FOLDER=${data_folder} ${CLITOOLS}/runhaskell -- -fdefer-type-errors -fno-ghci-sandbox $@"
fi
tab=`osascript -e 'tell app "Terminal" to do script "'"${command}"'"'`

osascript -e 'tell app "Terminal" to activate'
osascript -e 'tell app "Terminal" to set title displays window size of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set title displays file name of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set title displays device name of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set title displays shell path of current settings of '"${tab}"' to "No"'
osascript -e 'tell app "Terminal" to set custom title of '"${tab}"' to "Haskell for Mac: '"${executable}"'"'
