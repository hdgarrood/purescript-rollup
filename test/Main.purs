module Test.Main where

import Prelude
import Rollup as Rollup
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Monoid (mempty)
import Data.Options ((:=))
import Node.Process as Process

main :: Eff _ Unit
main = void $ runAff die (const (pure unit)) do
  bundle <- Rollup.rollup "./example/entry.js" mempty
  Rollup.write bundle "./test-output/test-bundle.js"
    (  Rollup.format := Rollup.UMD
    <> Rollup.moduleName := "TestMain"
    )

die ex = do
  log "failed:"
  log (show ex)
  Process.exit 1
