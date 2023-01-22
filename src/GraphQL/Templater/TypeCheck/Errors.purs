module GraphQL.Templater.TypeCheck.Errors where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST as AST
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import GraphQL.Templater.JsonPos (NormalizedJsonPos)
import GraphQL.Templater.Positions (Positions)

data TypeErrorWithPath a = TypeErrorWithPath (TypeError a) (List (NormalizedJsonPos a)) a

type PositionedError = TypeErrorWithPath Positions

derive instance Generic (TypeErrorWithPath a) _
derive instance Eq a => Eq (TypeErrorWithPath a)
derive instance Functor TypeErrorWithPath
instance Show a => Show (TypeErrorWithPath a) where
  show = genericShow

data TypeError a
  = FieldNotFound (Set String)
  | NotObject
  | ObjectWhenNodeExpected (Set String)
  | NotList
  | ArgTypeError (ArgTypeError a)

derive instance Functor TypeError
derive instance Generic (TypeError a) _
derive instance Eq a => Eq (TypeError a)
instance Show a => Show (TypeError a) where
  show = genericShow

data ArgTypeError a
  = ArgUnknown String a
  | ArgRequired String
  | ArgTypeMismatch
      { name :: String
      , definitionType :: AST.Type
      , argValue :: AST.Value
      , reasons :: NonEmptyList MismatchReason
      }
      a

derive instance Functor ArgTypeError
derive instance Generic (ArgTypeError a) _
derive instance Eq a => Eq (ArgTypeError a)
instance Show a => Show (ArgTypeError a) where
  show = genericShow

data MismatchReason = NullArgForNonNullType | ValueDoesNotFitDefinition

derive instance Generic MismatchReason _
derive instance Eq MismatchReason
instance Show MismatchReason where
  show = genericShow
