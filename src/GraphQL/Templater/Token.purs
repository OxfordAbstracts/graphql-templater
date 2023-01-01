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
  | OpenSquareBrackets
  | CloseSquareBrackets
  | Equals
  | Dot
  | Comma
  | Colon
  | Hash
  | Else
  | End
  | Null
  | Boolean Boolean
  | Int Int
  | Number Number
  | String String
  | WhiteSpace String
  | Literal String

type TokenWithPos = { start :: Position, end :: Position, token :: Token }

type Tokens = List TokenWithPos

derive instance Generic (Token) _

derive instance Eq (Token)

instance Show (Token) where
  show = genericShow
