module GraphQL.Templater.Ast where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST as GqlAst
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import GraphQL.Templater.Positions (Positions)

data Ast a
  = Var (VarPath a) a
  | Each (VarPath a) (List (Ast a)) a
  | Text String a

type AstPos = Ast Positions

derive instance Generic (Ast a) _

derive instance Functor Ast

instance Show a => Show (Ast a) where 
  show a = genericShow a

data VarPath a = VarPath (NonEmptyList (VarPathPart a)) a

derive instance Functor VarPath

instance Show a => Show (VarPath a) where 
  show a = genericShow a

derive instance Generic (VarPath a) _

data VarPathPart a = VarPathPart
  { name :: VarPartName a
  , args :: Maybe (Tuple GqlAst.Arguments a)
  }
  a
  

derive instance Functor VarPathPart

instance Show a => Show (VarPathPart a) where 
  show a = genericShow a

derive instance Generic (VarPathPart a) _

data VarPartName a
  = VarPartNameGqlName String a
  | VarPartNameParent a
  | VarPartNameRoot a

derive instance Functor VarPartName

derive instance Eq a => Eq (VarPartName a)

derive instance Ord a => Ord (VarPartName a)

instance Show a => Show (VarPartName a) where 
  show a = genericShow a

derive instance Generic (VarPartName a) _

