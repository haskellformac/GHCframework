#!/bin/sh

#  MakeHaskellCLIDefault.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 04.03.17.
#  Copyright © 2017 Manuel M T Chakravarty. All rights reserved.
#
#  This script links or unlinks the HaskellCLI binaries from /usr/local
#
#  * The first argument must be 'install' or 'uninstall'.

CLITOOLS=BIN_PATH

case $1 in

  install)
    ln -sfhF ${CLITOOLS}/* /usr/local/bin;;

  uninstall)
    for file in `ls /usr/local/bin`; do
      ls -l "/usr/local/bin/${file}" | grep ${CLITOOLS} >/dev/null
      if [ $? = 0 ]; then
        rm -f "/usr/local/bin/${file}"
      fi
    done;;

esac
