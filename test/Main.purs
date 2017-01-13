module Test.Main where

import Prelude
import Rollup as Rollup
import Control.Monad.Aff (launchAff, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Error.Class (throwError)
import Data.Monoid (mempty)
import Data.Options ((:=))

main :: Eff (err :: EXCEPTION, rollup :: Rollup.ROLLUP) Unit
main = void $ runAff throwException (const (pure unit)) do
  bundle <- Rollup.rollup "./example/entry.js" mempty
  Rollup.write bundle "./output/test-bundle.js" (Rollup.format := Rollup.UMD)
