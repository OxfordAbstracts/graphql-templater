module GraphQL.Templater.Ast
  ( Arg(..)
  , ArgName(..)
  , Args
  , Ast(..)
  , AstPos
  , Asts(..)
  , Value(..)
  , VarPartName(..)
  , VarPath(..)
  , VarPathPart(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST as GqlAst
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)
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

type Args a = List (Arg a)

data Arg a = Arg
  { name :: ArgName a
  , value :: Value a
  }
  a

derive instance Functor Arg
derive instance Foldable Arg
derive instance Traversable Arg
derive instance Eq a => Eq (Arg a)
derive instance Ord a => Ord (Arg a)
instance Show a => Show (Arg a) where
  show a = genericShow a

derive instance Generic (Arg a) _

data ArgName a = ArgName String a

derive instance Functor ArgName
derive instance Foldable ArgName
derive instance Traversable ArgName
derive instance Eq a => Eq (ArgName a)
derive instance Ord a => Ord (ArgName a)
instance Show a => Show (ArgName a) where
  show a = genericShow a

derive instance Generic (ArgName a) _

data Value a = Value GqlAst.Value a

derive instance Functor Value
derive instance Foldable Value
derive instance Traversable Value
derive instance Eq a => Eq (Value a)
derive instance Ord a => Ord (Value a)
instance Show a => Show (Value a) where
  show a = genericShow a

derive instance Generic (Value a) _
