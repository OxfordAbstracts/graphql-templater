module GraphQL.Templater.Token where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Parsing (Position)

data Token 
  = OpenBraces
  | CloseBraces
  | OpenParen
  | CloseParen
  | Equals
  | Dot
  | Comma
  | Colon
  | Hash
  | Else 
  | End
  | WhiteSpace String
  | Text String

type TokenWithPos = { start :: Position, end :: Position, token :: Token}

type Tokens = List TokenWithPos

derive instance Generic (Token) _

derive instance Eq (Token)

instance Show (Token) where
  show = genericShow
