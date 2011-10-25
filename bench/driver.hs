#!/usr/bin/env runghc

import System.Process
import System.Exit

main = do
    ExitSuccess <- rawSystem "ghc" (words "main.hs -i.. -O2 -outputdir outputdir -fforce-recomp")
    exitWith =<< rawSystem "./main" []
