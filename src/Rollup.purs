-- | PureScript bindings to Rollup's JavaScript API. Based on rollup v0.41.x.
-- | See https://github.com/rollup/rollup/wiki/JavaScript-API.
module Rollup
  ( RollupOpts
  , GenerateOpts
  , Bundle
  , SourceMap
  , Plugin
  , Format(..)
  , ROLLUP
  , rollup
  , plugins
  , cache
  , generate
  , write
  , format
  , moduleName
  , parseFormat
  , formatToString
  ) where

import Prelude
import Control.Promise as Promise
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise)
import Data.Foreign (Foreign)
import Data.Function.Eff (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Data.Function.Uncurried (Fn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Op (Op(..))
import Data.Options (Option, Options, opt, options, (:=))
import Unsafe.Coerce (unsafeCoerce)

-- | An effect for anything that Rollup can do.
foreign import data ROLLUP :: !

-- | A phantom type for use with Data.Options.
foreign import data RollupOpts :: *

-- | A file path: the entry point of the bundle.
entry :: Option RollupOpts String
entry = opt "entry"

cache :: Option RollupOpts Bundle
cache = opt "cache"

plugins :: Option RollupOpts (Array Plugin)
plugins = opt "plugins"

foreign import data Plugin :: Type

foreign import data Bundle :: Type

-- | Given an entry point and extra options, invoke 'rollup' to produce a bundle.
rollup :: forall e. String -> Options RollupOpts -> Aff (rollup :: ROLLUP | e) Bundle
rollup entryPoint opts =
  liftEff (runEffFn1 rollupImpl fullOpts) >>= Promise.toAff
  where
    fullOpts = options $ (entry := entryPoint) <> opts

-- | A phantom type for use with Data.Options.
foreign import data GenerateOpts :: *

-- | Possible formats for a generated bundle.
data Format
  = AMD
  | CJS
  | ES
  | IIFE
  | UMD

derive instance ordFormat :: Ord Format
derive instance eqFormat :: Eq Format
derive instance genericFormat :: Generic Format _

instance showFormat :: Show Format where
  show = genericShow

-- | Convert a Format to a String, in the form that rollup will understand.
formatToString :: Format -> String
formatToString = case _ of
  AMD -> "amd"
  CJS -> "cjs"
  ES -> "es"
  IIFE -> "iife"
  UMD -> "umd"

-- | Parse a Format from a String.
parseFormat :: String -> Maybe Format
parseFormat = case _ of
  "amd" -> Just AMD
  "cjs" -> Just CJS
  "es" -> Just ES
  "iife" -> Just IIFE
  "umd" -> Just UMD
  _ -> Nothing

-- | The type of module to produce.
format :: Option GenerateOpts Format
format = Op \fmt -> opt "format" := formatToString fmt

-- | The name to use for UMD/IIFE bundles.
moduleName :: Option GenerateOpts String
moduleName = opt "moduleName"

foreign import data SourceMap :: *

-- | Note: if your bundle has exports and you're using IIFE or UMD,
-- | the moduleName option is required. Also, the `map` property will be
-- | undefined if you do not specify the `sourceMap` option.
generate :: forall e.
  Bundle ->
  Options GenerateOpts ->
  Aff (rollup :: ROLLUP | e) { code :: String, map :: SourceMap }
generate bundle _ = pure { code: "", map: unsafeCoerce unit }

-- | Not strictly a 'generate' option, since this only makes sense
-- | with `write`, not `generate`. So we don't export this.
dest :: Option GenerateOpts String
dest = opt "dest"

-- | Write a bundle to a file.
write :: forall e.
  Bundle ->
  String ->
  Options GenerateOpts ->
  Aff (rollup :: ROLLUP | e) Unit
write bundle destPath opts =
  liftEff (runEffFn2 writeImpl bundle fullOpts) >>= Promise.toAff
  where
    fullOpts = options $ (dest := destPath) <> opts

foreign import rollupImpl :: forall e. EffFn1 (rollup :: ROLLUP | e) Foreign (Promise Bundle)

foreign import generateImpl :: Fn2 Bundle Foreign { code :: String, map :: String }

foreign import writeImpl :: forall e. EffFn2 (rollup :: ROLLUP | e) Bundle Foreign (Promise Unit)
