module Rollup.Plugin.PureScript where

import Prelude
import Rollup as Rollup
import Data.Foreign (Foreign)
import Data.Options (Option, Options, opt, options)

foreign import data PluginOpts :: *

outputDir :: Option PluginOpts String
outputDir = opt "outputDir"

runMain :: Option PluginOpts Boolean
runMain = opt "runMain"

uncurry :: Option PluginOpts Boolean
uncurry = opt "uncurry"

foreign import pluginImpl :: Foreign -> Rollup.Plugin

plugin :: Options PluginOpts -> Rollup.Plugin
plugin = pluginImpl <<< options
