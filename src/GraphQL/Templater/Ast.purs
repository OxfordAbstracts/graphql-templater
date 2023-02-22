module GraphQL.Templater.Ast
  ( Args
  , Ast(..)
  , AstPos
  , Asts(..)
  , VarPartName(..)
  , VarPath(..)
  , VarPathPart(..)
  , getPos
  , getVartPathPartName
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import GraphQL.Templater.Ast.Argument (Argument)
import GraphQL.Templater.Positions (Positions)

newtype Asts a = Asts (List (Ast a))

derive instance Generic (Asts a) _
derive instance Functor Asts
derive instance Foldable Asts
derive instance Traversable Asts
derive instance Eq a => Eq (Asts a)
derive instance Ord a => Ord (Asts a)
instance Show a => Show (Asts a) where
  show a = genericShow a

data Ast a
  = Var (VarPath a) a
  | Each (VarPath a) (List (Ast a)) a a
  | With (VarPath a) (List (Ast a)) a a
  | Text String a

getPos :: forall a. Ast a -> { open :: a, close :: Maybe a }
getPos = case _ of
  Var _ open -> { open, close: Nothing }
  Each _ _ open close -> { open, close: Just close }
  With _ _ open close -> { open, close: Just close }
  Text _ open -> { open, close: Nothing }

type AstPos = Ast Positions

derive instance Generic (Ast a) _
derive instance Functor Ast
derive instance Foldable Ast
derive instance Traversable Ast
derive instance Eq a => Eq (Ast a)
derive instance Ord a => Ord (Ast a)
instance Show a => Show (Ast a) where
  show a = genericShow a

data VarPath a = VarPath (NonEmptyList (VarPathPart a)) a

derive instance Functor VarPath
derive instance Foldable VarPath
derive instance Traversable VarPath
derive instance Eq a => Eq (VarPath a)
derive instance Ord a => Ord (VarPath a)
instance Show a => Show (VarPath a) where
  show a = genericShow a

derive instance Generic (VarPath a) _

data VarPathPart a = VarPathPart
  { name :: VarPartName a
  , args :: Maybe (Args a)
  }
  a

getVartPathPartName :: forall a. VarPathPart a -> VarPartName a
getVartPathPartName (VarPathPart { name } _) = name

derive instance Functor VarPathPart
derive instance Foldable VarPathPart
derive instance Traversable VarPathPart
derive instance Eq a => Eq (VarPathPart a)
derive instance Ord a => Ord (VarPathPart a)
derive instance Generic (VarPathPart a) _
instance Show a => Show (VarPathPart a) where
  show a = genericShow a

data VarPartName a
  = VarPartNameGqlName String a
  | VarPartNameParent a
  | VarPartNameRoot a

derive instance Functor VarPartName
derive instance Foldable VarPartName
derive instance Traversable VarPartName
derive instance Eq a => Eq (VarPartName a)
derive instance Ord a => Ord (VarPartName a)
instance Show a => Show (VarPartName a) where
  show a = genericShow a

derive instance Generic (VarPartName a) _

type Args a = List (Argument a)
