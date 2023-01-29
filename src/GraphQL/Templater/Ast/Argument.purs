module GraphQL.Templater.Ast.Argument where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

derive instance Generic (Value a) _

derive instance Functor Value

instance Show a => Show (Value a) where
  show v = genericShow v

derive instance Eq a => Eq (Value a)

derive instance Ord a => Ord (Value a)

instance (Eq a, Show a) => Hashable (Value a) where
  hash = show >>> hash

data Value a
  = Value_Variable Variable a
  | Value_IntValue IntValue a
  | Value_FloatValue FloatValue a
  | Value_StringValue StringValue a
  | Value_BooleanValue BooleanValue a
  | Value_NullValue NullValue a
  | Value_EnumValue EnumValue a
  | Value_ListValue (ListValue a) a
  | Value_ObjectValue (ObjectValue a) a

derive instance Generic IntValue _

instance Show IntValue where
  show v = genericShow v

derive instance Eq IntValue

derive instance Ord IntValue

_IntValue
  ∷ Tuple
      ( Int → IntValue
      )
      ( IntValue
        → Maybe Int
      )
_IntValue =
  Tuple IntValue
    ( case _ of
        IntValue a → Just a
    )

newtype IntValue = IntValue Int

derive instance Generic FloatValue _

instance Show FloatValue where
  show v = genericShow v

derive instance Eq FloatValue

derive instance Ord FloatValue

_FloatValue
  ∷ Tuple
      ( Number → FloatValue
      )
      ( FloatValue
        → Maybe Number
      )
_FloatValue =
  Tuple FloatValue
    ( case _ of
        FloatValue a → Just a
    )

newtype FloatValue = FloatValue Number

derive instance Generic BooleanValue _

instance Show BooleanValue where
  show v = genericShow v

derive instance Eq BooleanValue

derive instance Ord BooleanValue

_BooleanValue
  ∷ Tuple
      ( Boolean → BooleanValue
      )
      ( BooleanValue
        → Maybe Boolean
      )
_BooleanValue =
  Tuple BooleanValue
    ( case _ of
        BooleanValue a → Just a
    )

newtype BooleanValue = BooleanValue Boolean

derive instance Generic StringValue _

instance Show StringValue where
  show v = genericShow v

derive instance Eq StringValue

derive instance Ord StringValue

_StringValue
  ∷ Tuple
      ( String → StringValue
      )
      ( StringValue
        → Maybe String
      )
_StringValue =
  Tuple StringValue
    ( case _ of
        StringValue a → Just a
    )

newtype StringValue = StringValue String

derive instance Generic NullValue _

instance Show NullValue where
  show v = genericShow v

derive instance Eq NullValue

derive instance Ord NullValue

_NullValue
  ∷ Tuple
      ( Unit → NullValue
      )
      ( NullValue
        → Maybe Unit
      )
_NullValue =
  Tuple (\_ → NullValue)
    ( case _ of
        NullValue → Just unit
    )

data NullValue = NullValue

derive instance Generic EnumValue _

instance Show EnumValue where
  show v = genericShow v

derive instance Eq EnumValue

derive instance Ord EnumValue

_EnumValue
  ∷ Tuple
      ( String → EnumValue
      )
      ( EnumValue
        → Maybe String
      )
_EnumValue =
  Tuple EnumValue
    ( case _ of
        EnumValue a → Just a
    )

newtype EnumValue = EnumValue String

derive instance Generic (ListValue a) _

instance Show a => Show (ListValue a) where
  show v = genericShow v

derive instance Eq a => Eq (ListValue a)

derive instance Ord a => Ord (ListValue a)

_ListValue
  ∷ forall a
   . Tuple
       ( (List (Value a)) → (ListValue a)
       )
       ( ListValue a
         → Maybe (List (Value a))
       )
_ListValue =
  Tuple ListValue
    ( case _ of
        ListValue a → Just a
    )

newtype ListValue a = ListValue (List (Value a))

derive instance Functor ListValue

derive instance Generic (ObjectValue a) _

derive instance Functor ObjectValue

instance Show a => Show (ObjectValue a) where
  show v = genericShow v

derive instance Eq a => Eq (ObjectValue a)

derive instance Ord a => Ord (ObjectValue a)

_ObjectValue
  ∷ forall a
   . Tuple
       ( (List (Argument a)) → (ObjectValue a)
       )
       ( (ObjectValue a)
         → Maybe (List (Argument a))
       )
_ObjectValue =
  Tuple ObjectValue
    ( case _ of
        ObjectValue a → Just a
    )

newtype ObjectValue a = ObjectValue (List (Argument a))

derive instance Generic (Argument a) _

derive instance Functor Argument

instance Show a => Show (Argument a) where
  show v = genericShow v

derive instance Eq a => Eq (Argument a)

derive instance Ord a => Ord (Argument a)

derive newtype instance (Show a, Hashable a) => Hashable (Argument a)

type T_Argument a = { name :: String, value :: Value a, pos :: a }

newtype Argument :: Type -> Type
newtype Argument a = Argument (T_Argument a)

derive instance Generic Variable _

instance Show Variable where
  show v = genericShow v

derive instance Eq Variable

derive instance Ord Variable

newtype Variable = Variable String
