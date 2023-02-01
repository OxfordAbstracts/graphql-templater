module GraphQL.Templater.Ast.Argument where

import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))

derive instance Generic (Value a) _

derive instance Functor Value
derive instance Foldable Value
derive instance Traversable Value

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

derive instance Newtype IntValue _

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

derive instance Newtype FloatValue _

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

derive instance Newtype BooleanValue _

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

derive instance Newtype StringValue _

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

derive instance Newtype EnumValue _

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

derive instance Newtype (ListValue a) _

derive instance Functor ListValue
derive instance Foldable ListValue
derive instance Traversable ListValue

derive instance Generic (ObjectValue a) _

derive instance Functor ObjectValue
derive instance Foldable ObjectValue
derive instance Traversable ObjectValue

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

derive instance Newtype (ObjectValue a) _

derive instance Generic (Argument a) _

derive instance Functor Argument
derive instance Foldable Argument
derive instance Traversable Argument

instance Show a => Show (Argument a) where
  show v = genericShow v

derive instance Eq a => Eq (Argument a)

derive instance Ord a => Ord (Argument a)

derive newtype instance (Show a, Hashable a) => Hashable (Argument a)

type T_Argument a = { name :: ArgName a, value :: Value a, pos :: a }

derive instance Newtype (Argument a) _
newtype Argument a = Argument (T_Argument a)

derive instance Generic Variable _

instance Show Variable where
  show v = genericShow v

derive instance Eq Variable

derive instance Ord Variable

newtype Variable = Variable String

derive instance Newtype Variable _

derive instance Generic (ArgName a) _

instance Show a => Show (ArgName a) where
  show v = genericShow v

derive instance Eq a => Eq (ArgName a)

derive instance Ord a => Ord (ArgName a)

derive instance Functor ArgName
derive instance Foldable ArgName
derive instance Traversable ArgName

data ArgName a = ArgName String a

instance (Eq a, Show a) => Hashable (ArgName a) where
  hash = show >>> hash