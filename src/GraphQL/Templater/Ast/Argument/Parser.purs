module GraphQL.Templater.Ast.Argument.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, singleton, some)
import Data.Enum (toEnum)
import Data.Foldable (fold)
import Data.Int as DI
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (maybe)
import Data.Number as DN
import Data.String.CodePoints as CP
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import GraphQL.Templater.Ast.Argument (StringWith(..))
import GraphQL.Templater.Ast.Argument as AST
import GraphQL.Templater.Ast.ParseUtils (withPositions)
import GraphQL.Templater.Positions (Positions)
import Parsing (Parser, ParserT, fail)
import Parsing.Combinators (between, lookAhead, option, try, (<?>))
import Parsing.String (anyChar, char, string)
import Parsing.String.Basic (noneOf, oneOf)

-------
-- util
-------
c2str ∷ Char → Parser String String
c2str = pure <<< fromCharArray <<< singleton

ca2str ∷ Array Char → Parser String String
ca2str = pure <<< fromCharArray

toCA ∷ Char → Parser String (Array Char)
toCA = pure <<< singleton

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy_ :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy_ p sep = sepEndBy1_ p sep <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1_ :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy1_ p sep = do
  a <- p
  ( do
      _ <- sep
      as <- sepEndBy_ p sep
      pure (a : as)
  ) <|> pure (L.singleton a)

--------------
-- chars
--------------
upper :: Array Char
upper = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]

lower :: Array Char
lower = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

digits :: Array Char
digits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

nonZeroDigits :: Array Char
nonZeroDigits = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

--------------
-- ignore
--------------
-- also needs unicode bom for ignore, this will fail on unicode...
whitespace ∷ Parser String Unit
whitespace = void $ oneOf [ ' ', '\t' ]

comment ∷ Parser String Unit
comment = void $ char '#' *> many (noneOf [ '\n' ])

comma ∷ Parser String Unit
comma = void $ char ','

lineTerminator ∷ Parser String Unit
lineTerminator = void $ char '\n'

ignorable ∷ Parser String Unit
ignorable = lineTerminator <|> comma <|> comment <|> whitespace

ignoreMe ∷ Parser String Unit
ignoreMe = void $ many ignorable

ignoreMe' ∷ Parser String Unit
ignoreMe' = void $ some ignorable

--------------
-- primitives
--------------
name ∷ Parser String String
name =
  fromCharArray
    <$>
      ( (<>)
          <$> (oneOf (upper <> lower <> [ '_' ]) >>= toCA)
          <*> (many (oneOf $ upper <> lower <> digits <> [ '_' ]))
      )

description ∷ Parser String String
description = stringValue >>= (\(AST.StringValue s) → pure s)

negativeSign ∷ Parser String String
negativeSign = char '-' >>= c2str

ip0 ∷ Parser String String
ip0 = (<>) <$> (option "" negativeSign) <*> (char '0' >>= c2str)

ipOther ∷ Parser String String
ipOther =
  fold
    <$> sequence
      [ option "" negativeSign
      , oneOf nonZeroDigits >>= c2str
      , many (oneOf digits) >>= ca2str
      ]

integerPart ∷ Parser String String
integerPart = (try ip0) <|> ipOther

intValue ∷ Parser String AST.IntValue
intValue = integerPart >>= maybe (fail "String not an int") (pure <<< AST.IntValue) <<< DI.fromString

fractionalPart ∷ Parser String String
fractionalPart =
  (<>)
    <$> (char '.' >>= c2str)
    <*> (many (oneOf digits) >>= ca2str)

floatValueFrac ∷ Parser String String
floatValueFrac =
  (<>)
    <$> integerPart
    <*> fractionalPart

exponentPart ∷ Parser String String
exponentPart =
  fold
    <$> sequence
      [ oneOf [ 'e', 'E' ] >>= c2str
      , option "" (oneOf [ '+', '-' ] >>= c2str)
      , some (oneOf digits) >>= ca2str
      ]

floatValueExp ∷ Parser String String
floatValueExp = (<>) <$> integerPart <*> exponentPart

floatValueFracExp ∷ Parser String String
floatValueFracExp =
  fold
    <$> sequence [ integerPart, fractionalPart, exponentPart ]

floatValue ∷ Parser String AST.FloatValue
floatValue = (try floatValueFracExp <|> try floatValueExp <|> floatValueFrac) >>= maybe (fail "String not a float") (pure <<< AST.FloatValue) <<< DN.fromString

singleQuote ∷ Parser String String
singleQuote = char '"' >>= c2str

tripleQuote ∷ Parser String String
tripleQuote = sequence [ char '"', char '"', char '"' ] >>= ca2str

uni ∷ Parser String Char
uni = oneOf (digits <> [ 'A', 'B', 'C', 'D', 'E', 'F' ] <> [ 'a', 'b', 'c', 'd', 'e', 'f' ])

simpleUnescapedString ∷ Parser String String
simpleUnescapedString = noneOf [ '\\', '"', '\n' ] >>= c2str

simpleUnicodeString ∷ Parser String String
simpleUnicodeString =
  (sequence [ char '\\' *> char 'u' *> uni, uni, uni, uni ]) >>= ca2str
    >>=
      ( maybe
          (fail "Unrepresentable code point")
          ( maybe
              (fail "Unrepresentable code point")
              (pure <<< CP.singleton <<< CP.codePointFromChar)
              <<< toEnum
          )
          <<< DI.fromStringAs DI.hexadecimal
      )

simpleEscapedString ∷ Parser String String
simpleEscapedString =
  char '\\' *> oneOf [ '"', '\\', '/', 'b', 'f', 'n', 'r', 't' ]
    >>=
      ( \x → case x of
          '"' → pure '"'
          '\\' → pure '\\'
          '/' → pure '/'
          'n' → pure '\n'
          'r' → pure '\r'
          't' → pure '\t'
          'b' → fail "Cannot handle backspace yet"
          'f' → fail "Cannot handle formfeed yet"
          _ → fail "No clue how to parse this escapedString"
      )
    >>= c2str

simpleStringSingleton ∷ Parser String String
simpleStringSingleton =
  (try simpleUnescapedString)
    <|> (try simpleUnicodeString)
    <|> simpleEscapedString

simpleStringValue ∷ Parser String String
simpleStringValue =
  between singleQuote singleQuote
    ( fold
        <$> (many simpleStringSingleton)
    )

notTripleQuote ∷ Parser String String
notTripleQuote = (lookAhead (sequence [ anyChar, anyChar, anyChar ])) >>= (\s → if (s == [ '"', '"', '"' ]) then (fail "this is a triple quote") else anyChar >>= c2str)

blockStringValue ∷ Parser String String
blockStringValue = between tripleQuote tripleQuote (fold <$> many notTripleQuote)

stringValue ∷ Parser String AST.StringValue
stringValue = AST.StringValue <$> ((try blockStringValue) <|> simpleStringValue)

variable ∷ Parser String AST.Variable
variable = AST.Variable <$> (char '$' *> name)

booleanValue ∷ Parser String AST.BooleanValue
booleanValue = AST.BooleanValue <$> ((string "true" *> pure true) <|> (string "false" *> pure false))

nullValue ∷ Parser String AST.NullValue
nullValue = string "null" *> (pure AST.NullValue)

enumValue ∷ Parser String AST.EnumValue
enumValue =
  AST.EnumValue
    <$> (name >>= \x → if (x == "null" || x == "true" || x == "false") then fail "Name cannot be null, false or true" else pure x)

listValue ∷ Parser String (AST.Value Positions) → Parser String (AST.ListValue Positions)
listValue = (<$>) AST.ListValue <<< listish "[" "]"

argument ∷ Parser String (AST.Value Positions) → Parser String (AST.Argument Positions)
argument vc =
  map AST.Argument $ withPositions $ { name: _, value: _, pos: _ }
    <$> withPositions (StringWith <$> name)
    <*> (ignoreMe *> char ':' *> ignoreMe *> vc)

_listish ∷ ∀ p. Parser String p → Parser String (L.List p)
_listish p = sepEndBy_ p ignoreMe

_listish1 ∷ ∀ p. Parser String p → Parser String (L.List p)
_listish1 p = L.fromFoldable <$> sepEndBy1_ p ignoreMe

listish ∷ ∀ p. String → String → Parser String p → Parser String (L.List p)
listish o c p = string o *> ignoreMe *> _listish p <* string c

objectValue ∷ Parser String (AST.Value Positions) → Parser String (AST.ObjectValue Positions)
objectValue = (<$>) AST.ObjectValue <<< listish "{" "}" <<< argument

value ∷ Parser String (AST.Value Positions)
value =
  fix \p →
    withPositions $
      (try (AST.Value_Variable <$> variable))
        <|> (try (AST.Value_NullValue <$> nullValue))
        <|> (try (AST.Value_BooleanValue <$> booleanValue))
        <|> (try (AST.Value_StringValue <$> stringValue))
        <|> (try (AST.Value_FloatValue <$> floatValue))
        <|> (try (AST.Value_IntValue <$> intValue))
        <|> (try (AST.Value_EnumValue <$> enumValue))
        <|> (try (AST.Value_ListValue <$> listValue p))
        <|> (AST.Value_ObjectValue <$> objectValue p)
          <?> "value"
