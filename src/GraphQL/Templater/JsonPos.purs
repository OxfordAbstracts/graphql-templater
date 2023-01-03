module GraphQL.Templater.JsonPos where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST (Arguments)
import Data.List (List(..), dropWhile, tail, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import GraphQL.Templater.Ast (VarPartName(..), VarPathPart(..))
import GraphQL.Templater.Eval.MakeQuery (getAlias, nilArgs)

data JsonPos a
  = Parent a
  | Root a
  | Pos (NormalizedJsonPos a)

data NormalizedJsonPos a = Key String a | Index Int a

varPathToPosition :: forall f a. Foldable f => f (VarPathPart a) -> List (JsonPos a)
varPathToPosition path = foldl step Nil path
  where
  step res (VarPathPart { name, args } _) = case name of
    VarPartNameRoot a -> Root a : res
    VarPartNameParent a -> Parent a : res
    VarPartNameGqlName gqlName a ->
      (Pos $ Key (getKey (maybe nilArgs fst args) gqlName) a)
        : res

normalizePos :: forall a. List (JsonPos a) -> List (NormalizedJsonPos a)
normalizePos = go Nil
  where
  go acc = case _ of
    Nil -> acc
    Cons (Parent _) t -> go acc (fromMaybe Nil $ tail (dropWhile isIndex t))
    Cons (Root _) _t -> go acc Nil
    Cons (Pos pos) t -> go (Cons pos acc) t

  isIndex = case _ of
    Pos (Index _ _) -> true
    _ -> false

addJsonIdx :: forall a. Int -> List (JsonPos a) -> List (JsonPos a)
addJsonIdx idx l = case uncons l of
  Just { head: Pos (Key key a), tail } -> (Pos $ Index idx a) : (Pos (Key key a)) : tail
  _ -> l

getKey ∷ Arguments → String → String
getKey args name = fromMaybe name $ getAlias name args

derive instance Generic (JsonPos a) _
derive instance Functor JsonPos

instance Show a => Show (JsonPos a) where
  show = genericShow

derive instance Functor NormalizedJsonPos

derive instance Generic (NormalizedJsonPos a) _

instance Show a => Show (NormalizedJsonPos a) where
  show = genericShow