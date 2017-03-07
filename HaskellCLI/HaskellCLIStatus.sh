#!/bin/sh

#  HaskellCLIStatus.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 06.03.17.
#  Copyright © 2017 Manuel M T Chakravarty. All rights reserved.
#
#  Determines whether the correct version of the Haskell CLI is installed and, if so, ensures that is is properly set
#  up.

CLITOOLS=BIN_PATH

${CLITOOLS}/../ghc/SetupCLI ${CLITOOLS}/bin >/dev/null 2>/dev/null
setup_exit=$?
ghc_framework_version=`head -1 ${CLITOOLS}/../ghc/cabal.config | sed -e 's/^.*GHC.framework //'`
ghc_framework_build=`head -1 ${CLITOOLS}/../ghc/Version`
echo "${ghc_framework_version} (${ghc_framework_build})"
exit ${setup_exit}
