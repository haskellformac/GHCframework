Haskell for Mac
===============

A simple Haskell IDE for OS X

# Building

Building happens in two stages: (1) The GHCBuild project is used to build the GHC distribution and install all required packages into its global package database and library directory. (2) The HaskellForMac project builds Haskell.app including all its dependencies (except for GHCBuild, which must have been built seperately). This process applies to Debug and to Release builds.

This decomposition is necessary as GHC builds are very time consuming, but change very rarely. We want to be able to clean HaskellForMac and build from scratch without rebuilding GHC.

GHCBuild builds GHCBuild.bundle, whereas HaskellForMac depends on the GHC project, which builds GHC.framework by copying the contents of GHCBuild.bundle and determining which flavour of the GHC RTS ought to be (dynamically) linked into the main Haskell.app executable. The RTS library is being linked by linking against GHC.framework.
