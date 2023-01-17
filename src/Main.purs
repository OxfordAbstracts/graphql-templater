module Main (main) where

import Prelude

import Effect (Effect)
import GraphQL.Templater.View.App as App
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI App.component unit body
