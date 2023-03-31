module GraphQL.Templater.JsonPos where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), dropWhile, tail, uncons, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import GraphQL.Templater.Ast (VarPartName(..), VarPathPart(..), Args)
import GraphQL.Templater.Eval.MakeQuery (getAlias)

data JsonPos a
  = Parent a
  | Root a
  | Pos (NormalizedJsonPos a)

data NormalizedJsonPos a
  = Key
      { name :: String
      , alias :: Maybe String
      }
      a
  | Index Int a

getJsonPosArg :: forall a. JsonPos a -> a
getJsonPosArg = case _ of
  Parent a -> a
  Root a -> a
  Pos (Key _ a) -> a
  Pos (Index _ a) -> a

varPathToPosition :: forall f a. Foldable f => f (VarPathPart a) -> List (JsonPos a)
varPathToPosition path = foldl step Nil path
  where
  step res (VarPathPart { name, args } _) = case name of
    VarPartNameRoot a -> Root a : res
    VarPartNameParent a -> Parent a : res
    VarPartNameGqlName gqlName a ->
      (Pos $ Key (getKey (fromMaybe Nil args) gqlName) a)
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

getKey ∷ forall a. Args a → String → { name :: String, alias :: Maybe String }
getKey args name = { name, alias: getAlias name args }

getKeyStr :: { name :: String, alias :: Maybe String } -> String
getKeyStr key = fromMaybe key.name key.alias

derive instance Generic (JsonPos a) _

derive instance Functor JsonPos
derive instance Foldable JsonPos
derive instance Traversable JsonPos

derive instance Eq a => Eq (JsonPos a)

instance Show a => Show (JsonPos a) where
  show = genericShow

derive instance Functor NormalizedJsonPos
derive instance Traversable NormalizedJsonPos
derive instance Foldable NormalizedJsonPos

derive instance Generic (NormalizedJsonPos a) _

derive instance Eq a => Eq (NormalizedJsonPos a)

instance Show a => Show (NormalizedJsonPos a) where
  show = genericShow