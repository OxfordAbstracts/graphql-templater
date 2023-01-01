module GraphQL.Templater.Eval.Interpolate
  ( interpolate
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJson, caseJsonArray, caseJsonObject, jsonNull)
import Data.Array ((!!))
import Data.Foldable (class Foldable, fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST (Arguments)
import Data.Int (round, toNumber)
import Data.List (List(..), foldMap, foldl, tail, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Foreign.Object as Object
import GraphQL.Templater.Ast (Ast, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Ast as Ast
import GraphQL.Templater.Eval.MakeQuery (getAlias, nilArgs)

interpolate :: forall a. List (Ast a) -> Json -> String
interpolate = go Nil
  where
  go :: List JsonPos -> List (Ast a) -> Json -> String
  go path asts json = asts # foldMap mergeAst
    where

    mergeAst = case _ of
      Ast.Text s _ -> s
      Ast.Var (VarPath v _) _ -> lookupJsonD v # displayJson path
      Ast.Each (VarPath v _) asts' _ ->
        let
          arr = lookupJson v # caseJsonArray [] identity
        in
          fold $ arr # mapWithIndex \idx _json' ->
            go (addJsonIdx idx $ varPathToPosition v <> path) asts' json

      where
      lookupJson v = foldl step json fullPath
        where
        step currentJson = case _ of
          Key key -> lookupObj key currentJson
          Index key idx -> lookupObj key currentJson # lookupArr idx

        fullPath = normalizePos $ varPathToPosition v <> path

        lookupObj key = caseJsonObject jsonNull \obj ->
          fromMaybe jsonNull $ Object.lookup key obj

        lookupArr idx = caseJsonArray jsonNull \arr ->
          fromMaybe jsonNull $ arr !! idx

      lookupJsonD v = foldl step json fullPath
        where
        step currentJson pos = case pos of
          Key key -> lookupObj key currentJson
          Index key idx -> lookupObj key currentJson # lookupArr idx

        fullPath = normalizePos $ varPathToPosition v <> path

        lookupObj key = caseJsonObject jsonNull \obj ->
          fromMaybe jsonNull $ Object.lookup key obj

        lookupArr idx = caseJsonArray jsonNull \arr ->
          fromMaybe jsonNull $ arr !! idx

varPathToPosition :: forall f a. Foldable f => f (VarPathPart a) -> List JsonPos
varPathToPosition path = foldl step Nil path
  where
  step res (VarPathPart { name, args } _) = case name of
    VarPartNameRoot _ -> Root : res
    VarPartNameParent _ -> Parent : res
    VarPartNameGqlName gqlName _ ->
      (Pos $ Key $ getKey (maybe nilArgs fst args) gqlName)
        : res

addJsonIdx :: Int -> List JsonPos -> List JsonPos
addJsonIdx idx l = case uncons l of
  Just { head: Pos (Key key), tail } -> Cons (Pos $ Index key idx) tail
  _ -> l

getKey ∷ Arguments → String → String
getKey args name = fromMaybe name $ getAlias name args

data JsonPos
  = Parent
  | Root
  | Pos NormalizedJsonPos

derive instance Generic JsonPos _

instance Show JsonPos where
  show = genericShow

data NormalizedJsonPos = Key String | Index String Int

derive instance Generic NormalizedJsonPos _

instance Show NormalizedJsonPos where
  show = genericShow

normalizePos :: List JsonPos -> List NormalizedJsonPos
normalizePos = go Nil
  where
  go acc = case _ of
    Nil -> acc
    Cons Parent t -> go acc (fromMaybe Nil $ tail t)
    Cons Root _t -> go acc Nil
    Cons (Pos pos) t -> go (Cons pos acc) t

displayJson :: List JsonPos -> Json -> String
displayJson path j = caseJson
  (const "")
  show
  (\n -> if toNumber (round n) == n then show (round n) else show n)
  identity
  (foldMap $ displayJson path)
  (\_ -> show path)
  j