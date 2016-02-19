{- cabal setup file -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

import Distribution.Simple
import Distribution.Simple.Setup

import qualified Build.Configure as Configure

main = defaultMainWithHooks simpleUserHooks
	{ preConf = configure
	}

configure _ _ = do
	Configure.run Configure.tests
	return (Nothing, [])
