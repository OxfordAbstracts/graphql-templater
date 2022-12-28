module GraphQL.Templater.Parser where

import Prelude hiding (when)

import Data.List (List)
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Ast(..), AstPos, VarIdent(..))
-- import GraphQL.Templater.Token (TokenPos, getTokenVar)
import GraphQL.Templater.Token as T
import Parsing (Parser, ParserT, fail)
import Parsing.Combinators (try, (<|>))
import Parsing.Token (token, when)
import Unsafe.Coerce (unsafeCoerce)

-- parser :: Parser (List TokenPos) AstPos
-- parser =  ifP <|> p 

--   where
--   p = mayTok case _ of
--     T.Var a b -> Just $ pure $ Var a b
--     T.Text a b -> Just $ pure $ Text a b
--     _ -> Nothing

--   ifP = do
--     ifStr <- try $ mayTok \t -> case t of
--       T.If s _ -> Just s
--       _ -> Nothing

--     --  token $ _.start <<< getTokenVar
--     pure $ If
--       { condition: unsafeCoerce unit, then: unsafeCoerce unit, else: Nothing }
--       (unsafeCoerce unit)

-- varIdentParser :: Parser (List TokenPos) VarIdent
-- varIdentParser = unsafeCoerce unit

-- -- getTok 

-- whenTok :: forall m. (TokenPos -> Boolean) -> ParserT (List TokenPos) m TokenPos
-- whenTok = when (_.start <<< getTokenVar)

-- mayTok :: forall m a. (TokenPos -> Maybe (ParserT (List TokenPos) m a)) -> ParserT (List TokenPos) m a
-- mayTok fn = do
--   t <- token (_.start <<< getTokenVar)
--   case fn t of
--     Just a ->  a
--     Nothing -> fail "Unexpected token"

-- cond <- parser
-- _ <- token "then"
-- then <- parser
-- else <- optional $ do
--   _ <- token "else"
--   parser
-- pure $ If cond then else