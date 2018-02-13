#!/bin/sh

#  RunCLICommand.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 28.02.17.
#  Copyright © [2017..2018] Manuel M T Chakravarty. All rights reserved.
#
#  Run a command of the Haskell CLI outside of the HfM sandbox. (The command is run in a temporary directory, just in case.)
#
#  * The first argument is 'none' or a directory.
#  * The second argument is the command to execute.
#  * The remaining arguments are to be passed to that command.

CLITOOLS=BIN_PATH

dirArg="$1"
shift
command=$1
shift

if [ "$dirArg" = "none" ]; then
  dir=`mktemp -d -t runcli` || exit 1
else
  dir="$dirArg"
fi

echo cd "${dir}"
cd "${dir}"
if [ -x ${CLITOOLS}/${command} ]; then

  # Don't exec, so that 'StopCLICommand' can do its job.
  echo ${CLITOOLS}/${command} "$@"
  ${CLITOOLS}/${command} "$@"

else

  echo "No such Haskell CLI command: ${command}"
  exit 1

fi
