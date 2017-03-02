#!/bin/sh

#  RunCLICommand.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 28.02.17.
#  Copyright © 2017 Manuel M T Chakravarty. All rights reserved.
#
#  Run a command of the Haskell CLI outside of the HfM sandbox. (The command is run in a temporary directory, just in case.)
#
#  * The first argument is the command to execute.
#  * The remaining arguments are to be passed to that command.

CLITOOLS=BIN_PATH

command=$1
shift

tmp=`mktemp -d -t runcli` || exit 1

cd ${tmp}
exec ${CLITOOLS}/${command} "$@"
