module GraphQL.Templater.Todo where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Warn, Text)

todo :: Warn (Text "TODO") => forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg