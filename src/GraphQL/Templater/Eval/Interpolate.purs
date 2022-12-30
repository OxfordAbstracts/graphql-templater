module GraphQL.Templater.Eval.Interpolate
  ( interpolate
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJson, caseJsonArray, caseJsonObject, fromObject, jsonNull, stringify)
import Data.Array ((!!))
import Data.Foldable (class Foldable, fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.GraphQL.AST (Arguments)
import Data.Int (round, toNumber)
import Data.List (List(..), foldMap, foldl, tail, (:))
import Data.Maybe (fromMaybe, maybe)
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
      Ast.Var (VarPath v _) _ -> lookupJson v # displayJson
      Ast.Each (VarPath v _) asts' _ ->
        let
          arr = lookupJson v # caseJsonArray [] identity
        in
          fold $ arr # mapWithIndex \idx _json' ->
            go ((Pos $ Index idx) : varPathToPosition v <> path) asts' json

      where
      lookupJson v = foldl step json fullPath
        where
        fullPath = normalizePos $ varPathToPosition v <> path

        step currentJson = case _ of
          Key key -> currentJson # caseJsonObject jsonNull \obj ->
            fromMaybe jsonNull $ Object.lookup key obj
          Index idx -> currentJson # caseJsonArray jsonNull \arr ->
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

getKey ∷ Arguments → String → String
getKey args name = fromMaybe name $ getAlias name args

data JsonPos
  = Parent
  | Root
  | Pos NormalizedJsonPos

data NormalizedJsonPos = Key String | Index Int

normalizePos :: List JsonPos -> List NormalizedJsonPos
normalizePos = go Nil
  where
  go acc = case _ of
    Nil -> acc
    Cons Parent t -> go Nil t
    Cons Root t -> go (fromMaybe Nil $ tail acc) t
    Cons (Pos pos) t -> go (Cons pos acc) t

displayJson :: Json -> String
displayJson j = caseJson
  (const "")
  show
  (\n -> if toNumber (round n) == n then show (round n) else show n)
  identity
  (foldMap displayJson)
  (fromObject >>> stringify)
  j