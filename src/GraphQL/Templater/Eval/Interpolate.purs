module GraphQL.Templater.Eval.Interpolate
  ( interpolate
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJson, caseJsonArray, jsonNull)
import Data.Array ((!!))
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (round, toNumber)
import Data.List (List(..), foldMap, foldl)
import Data.Maybe (fromMaybe, maybe)
import Foreign.Object as Object
import GraphQL.Templater.Ast (Ast, VarPath(..))
import GraphQL.Templater.Ast as Ast
import GraphQL.Templater.JsonPos (JsonPos, NormalizedJsonPos(..), addJsonIdx, normalizePos, varPathToPosition)

interpolate :: forall a. List (Ast a) -> Json -> String
interpolate = go Nil
  where
  go :: List (JsonPos a) -> List (Ast a) -> Json -> String
  go path asts json = asts # foldMap mergeAst
    where

    mergeAst = case _ of
      Ast.Text s _ -> s
      Ast.Var (VarPath v _) _ -> lookupJson v # displayJson path
      Ast.Each (VarPath v _) asts' _ ->
        let
          arr = lookupJson v # caseJsonArray [] identity
        in
          fold $ arr # mapWithIndex \idx _json' ->
            go (addJsonIdx idx $ varPathToPosition v <> path) asts' json
      Ast.With (VarPath v _) asts' _ ->
        go (varPathToPosition v <> path) asts' json

      where
      lookupJson v = foldl step json fullPath
        where
        step currentJson = case _ of
          Key key _ -> lookupObj key currentJson
          Index idx _ -> currentJson # lookupArr idx

        fullPath = normalizePos $ varPathToPosition v <> path

        lookupObj key = caseJson
          (const jsonNull)
          (const jsonNull)
          (const jsonNull)
          (const jsonNull)
          (\arr -> maybe jsonNull (lookupObj key) $ arr !! 0)
          \obj ->
            fromMaybe jsonNull $ Object.lookup key obj

        lookupArr idx = caseJsonArray jsonNull \arr ->
          fromMaybe jsonNull $ arr !! idx

displayJson :: forall a. List (JsonPos a) -> Json -> String
displayJson path j = caseJson
  (const "")
  show
  (\n -> if toNumber (round n) == n then show (round n) else show n)
  identity
  (foldMap $ displayJson path)
  (\_ -> show (map (const unit) path))
  j