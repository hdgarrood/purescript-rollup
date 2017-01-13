module Test.Main where

import Prelude
import Node.Process as Process
import Rollup as Rollup
import Rollup.Plugin.PureScript as Purs
import Rollup.Plugin.NodeResolve as NodeResolve
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Monoid (mempty)
import Data.Options ((:=))

main :: Eff _ Unit
main = void $ runAff die (const (pure unit)) do
  testNoPlugins
  testPureScript

testNoPlugins = do
  bundle <- Rollup.rollup "./example/vanilla/entry.js" mempty
  Rollup.write bundle "./test-output/vanilla.js"
    (  Rollup.format := Rollup.UMD
    <> Rollup.moduleName := "Vanilla"
    )

purs =
  Purs.plugin (  Purs.outputDir := "./example/purescript"
              <> Purs.runMain := true
              )

nodeResolve =
  NodeResolve.plugin unit

testPureScript = do
  bundle <- Rollup.rollup "./example/purescript/Main/index.js"
              (Rollup.plugins := [purs, nodeResolve])
  Rollup.write bundle "./test-output/purescript.js"
    (  Rollup.format := Rollup.IIFE
    <> Rollup.moduleName := "PS"
    )

die ex = do
  log "failed:"
  log (show ex)
  Process.exit 1
