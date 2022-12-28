module GraphQL.Templater.Ast where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import GraphQL.Templater.Positions (Positions)

data Ast a
  = If
      { condition :: (Ast a)
      , then :: (Ast a)
      , else :: Maybe (Ast a)
      }
      a
  | Var (List VarIdent) a
  | Text String a

type AstPos = Ast Positions

derive instance Functor Ast

data VarIdent = VarIdent
  { name :: String
  , args ::
      List
        { name :: String
        , value :: String
        }
  }
