module GraphQL.Templater.Ast.GqlValue where

import Prelude
import Prim hiding (Type)

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

derive instance valueGeneric ∷ Generic Value _

instance valueShow ∷ Show Value where
  show v = genericShow v

instance EncodeJson Value where 
  encodeJson a = genericEncodeJson a
  
instance DecodeJson Value where 
  decodeJson a = genericDecodeJson a

derive instance valueEq ∷ Eq Value

derive instance valueOrd ∷ Ord Value

instance Hashable Value where 
  hash = show >>> hash


data Value
  = Value_Variable Variable
  | Value_IntValue IntValue
  | Value_FloatValue FloatValue
  | Value_StringValue StringValue
  | Value_BooleanValue BooleanValue
  | Value_NullValue NullValue
  | Value_EnumValue EnumValue
  | Value_ListValue ListValue
  | Value_ObjectValue ObjectValue

derive instance intValueGeneric ∷ Generic IntValue _

instance intValueShow ∷ Show IntValue where
  show v = genericShow v

instance EncodeJson IntValue where 
  encodeJson a = genericEncodeJson a

instance DecodeJson IntValue where 
  decodeJson a = genericDecodeJson a

derive instance intValueEq ∷ Eq IntValue

derive instance intValueOrd ∷ Ord IntValue

_IntValue ∷
  Tuple
    ( Int → IntValue
    )
    ( IntValue →
      Maybe Int
    )
_IntValue =
  Tuple IntValue
    ( case _ of
        IntValue a → Just a
    )

derive instance intValueNewtype ∷ Newtype IntValue _

newtype IntValue
  = IntValue Int

derive instance floatValueGeneric ∷ Generic FloatValue _

instance floatValueShow ∷ Show FloatValue where
  show v = genericShow v

instance EncodeJson FloatValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson FloatValue where 
  decodeJson a = genericDecodeJson a

derive instance floatValueEq ∷ Eq FloatValue

derive instance floatValueOrd ∷ Ord FloatValue

_FloatValue ∷
  Tuple
    ( Number → FloatValue
    )
    ( FloatValue →
      Maybe Number
    )
_FloatValue =
  Tuple FloatValue
    ( case _ of
        FloatValue a → Just a
    )

derive instance floatValueNewtype ∷ Newtype FloatValue _

newtype FloatValue
  = FloatValue Number

derive instance booleanValueGeneric ∷ Generic BooleanValue _

instance booleanValueShow ∷ Show BooleanValue where
  show v = genericShow v

instance EncodeJson BooleanValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson BooleanValue where 
  decodeJson a = genericDecodeJson a

derive instance booleanValueEq ∷ Eq BooleanValue

derive instance booleanValueOrd ∷ Ord BooleanValue

_BooleanValue ∷
  Tuple
    ( Boolean → BooleanValue
    )
    ( BooleanValue →
      Maybe Boolean
    )
_BooleanValue =
  Tuple BooleanValue
    ( case _ of
        BooleanValue a → Just a
    )

derive instance booleanValueNewtype ∷ Newtype BooleanValue _

newtype BooleanValue
  = BooleanValue Boolean

derive instance stringValueGeneric ∷ Generic StringValue _

instance stringValueShow ∷ Show StringValue where
  show v = genericShow v

instance EncodeJson StringValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson StringValue where 
  decodeJson a = genericDecodeJson a

derive instance stringValueEq ∷ Eq StringValue

derive instance stringValueOrd ∷ Ord StringValue

_StringValue ∷
  Tuple
    ( String → StringValue
    )
    ( StringValue →
      Maybe String
    )
_StringValue =
  Tuple StringValue
    ( case _ of
        StringValue a → Just a
    )

derive instance stringValueNewtype ∷ Newtype StringValue _

newtype StringValue
  = StringValue String

derive instance nullValueGeneric ∷ Generic NullValue _

instance nullValueShow ∷ Show NullValue where
  show v = genericShow v

instance EncodeJson NullValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson NullValue where 
  decodeJson a = genericDecodeJson a

derive instance nullValueEq ∷ Eq NullValue

derive instance nullValueOrd ∷ Ord NullValue

_NullValue ∷
  Tuple
    ( Unit → NullValue
    )
    ( NullValue →
      Maybe Unit
    )
_NullValue =
  Tuple (\_ → NullValue)
    ( case _ of
        NullValue → Just unit
    )

data NullValue
  = NullValue

derive instance enumValueGeneric ∷ Generic EnumValue _

instance enumValueShow ∷ Show EnumValue where
  show v = genericShow v

instance EncodeJson EnumValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson EnumValue where 
  decodeJson a = genericDecodeJson a

derive instance enumValueEq ∷ Eq EnumValue

derive instance enumValueOrd ∷ Ord EnumValue

_EnumValue ∷
  Tuple
    ( String → EnumValue
    )
    ( EnumValue →
      Maybe String
    )
_EnumValue =
  Tuple EnumValue
    ( case _ of
        EnumValue a → Just a
    )

derive instance enumValueNewtype ∷ Newtype EnumValue _

newtype EnumValue
  = EnumValue String

derive instance listValueGeneric ∷ Generic ListValue _

instance listValueShow ∷ Show ListValue where
  show v = genericShow v

instance EncodeJson ListValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson ListValue where 
  decodeJson a = genericDecodeJson a
derive instance listValueEq ∷ Eq ListValue

derive instance listValueOrd ∷ Ord ListValue

_ListValue ∷
  Tuple
    ( (List Value) → ListValue
    )
    ( ListValue →
      Maybe (List Value)
    )
_ListValue =
  Tuple ListValue
    ( case _ of
        ListValue a → Just a
    )

derive instance listValueNewtype ∷ Newtype ListValue _

newtype ListValue
  = ListValue (List Value)

derive instance objectValueGeneric ∷ Generic ObjectValue _

instance objectValueShow ∷ Show ObjectValue where
  show v = genericShow v

instance EncodeJson ObjectValue where 
  encodeJson a = genericEncodeJson a 

instance DecodeJson ObjectValue where 
  decodeJson a = genericDecodeJson a

derive instance objectValueEq ∷ Eq ObjectValue

derive instance objectValueOrd ∷ Ord ObjectValue

_ObjectValue ∷
  Tuple
    ( (List Argument) → ObjectValue
    )
    ( ObjectValue →
      Maybe (List Argument)
    )
_ObjectValue =
  Tuple ObjectValue
    ( case _ of
        ObjectValue a → Just a
    )

derive instance objectValueNewtype ∷ Newtype ObjectValue _

newtype ObjectValue
  = ObjectValue (List Argument)


derive instance argumentGeneric ∷ Generic Argument _

instance argumentShow ∷ Show Argument where
  show v = genericShow v

instance EncodeJson Argument where 
  encodeJson a = genericEncodeJson a

instance DecodeJson Argument where 
  decodeJson a = genericDecodeJson a

derive instance argumentEq ∷ Eq Argument

derive instance argumentOrd ∷ Ord Argument

derive instance Newtype Argument _

derive newtype instance Hashable Argument

type T_Argument
  = { name :: String, value :: Value }

_Argument ∷
  Tuple
    ( T_Argument → Argument
    )
    ( Argument →
      Maybe T_Argument
    )
_Argument =
  Tuple Argument
    ( case _ of
        Argument a → Just a
    )

newtype Argument
  = Argument T_Argument


derive instance variableGeneric ∷ Generic Variable _

instance variableShow ∷ Show Variable where
  show v = genericShow v

instance EncodeJson Variable where 
  encodeJson a = genericEncodeJson a

instance DecodeJson Variable where 
  decodeJson a = genericDecodeJson a

derive instance variableEq ∷ Eq Variable

derive instance variableOrd ∷ Ord Variable

_Variable ∷
  Tuple
    ( String → Variable
    )
    ( Variable →
      Maybe String
    )
_Variable =
  Tuple Variable
    ( case _ of
        Variable a → Just a
    )

derive instance variableNewtype ∷ Newtype Variable _

newtype Variable
  = Variable String
