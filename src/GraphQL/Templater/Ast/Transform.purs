module GraphQL.Templater.Ast.Transform
  ( insertEachOfPathAt
  , insertEmptyEachAt
  , insertEmptyEachAt'
  , insertEmptyWithAt'
  , insertSingleVarAt
  , insertTextAt
  , insertVarAt'
  , insertVarPathAt
  , insertWithOfPathAt
  , justPos
  , modifyAstStartingAt
  , modifyTextAt
  , nothingPos
  )
  where

import Prelude

import Data.Either (hush)
import Data.Foldable (foldl)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..), getPos)
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.Print (printPositionedMb, printUnpositioned)
import GraphQL.Templater.Ast.Suggest (getStartIdx)
import GraphQL.Templater.Positions (Positions)
import Record (merge)
import Record.Extra (pick)

insertTextAt
  :: String
  -> Int
  -> List (Ast Positions)
  -> Maybe (List (Ast Positions))
insertTextAt text idx =
  intersperseEmptyText
    >>> modifyTextAt go idx
    >>> map filterEmptyText
  where
  go existing positions@{ start } =
    let
      { before, after } = String.splitAt (idx - start) existing
      new = before <> text <> after
    in
      Text new positions
        { str = new
        , end = start + String.length new
        } : Nil

insertEmptyEachAt :: String -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertEmptyEachAt field = insertEmptyEachAt'
  ( pure $ VarPathPart
      { args: Nothing
      , name: VarPartNameGqlName field unit
      }
      unit
  )

insertEachOfPathAt :: NonEmptyList String -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertEachOfPathAt path = insertEmptyEachAt'
  ( path <#> \field -> VarPathPart
      { args: Nothing
      , name: VarPartNameGqlName field unit
      }
      unit
  )

insertWithOfPathAt :: NonEmptyList String -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertWithOfPathAt path = insertEmptyWithAt'
  ( path <#> \field -> VarPathPart
      { args: Nothing
      , name: VarPartNameGqlName field unit
      }
      unit
  )

insertEmptyEachAt' :: NonEmptyList (VarPathPart Unit) -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertEmptyEachAt' varPath = insertTextAt
  ( printUnpositioned
      $ pure
      $ Each (VarPath varPath unit) Nil unit unit
  )

insertEmptyWithAt' :: NonEmptyList (VarPathPart Unit) -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertEmptyWithAt' varPath = insertTextAt
  ( printUnpositioned
      $ pure
      $ With (VarPath varPath unit) Nil unit unit
  )

insertSingleVarAt :: String -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertSingleVarAt field = insertVarAt'
  ( pure $ VarPathPart
      { args: Nothing
      , name: VarPartNameGqlName field unit
      }
      unit
  )

insertVarPathAt :: NonEmptyList String -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertVarPathAt path = insertVarAt'
  ( path <#> \field -> VarPathPart
      { args: Nothing
      , name: VarPartNameGqlName field unit
      }
      unit
  )

insertVarAt' :: NonEmptyList (VarPathPart Unit) -> Int -> List (Ast Positions) -> Maybe (List (Ast Positions))
insertVarAt' varPath = insertTextAt
  ( printUnpositioned
      $ pure
      $ Var (VarPath varPath unit) unit
  )

nothingPos :: forall a p f. Functor f => f a -> f (Maybe p)
nothingPos = map (const Nothing)


-- justPos :: forall a p f. Functor f => f a -> f (Maybe p)
justPos :: forall f f2 a. Functor f => Functor f2 => f (f2 a) -> f (f2 (Maybe a))
justPos = map (map Just)

modifyAstStartingAt :: (Ast Positions -> List (Ast (Maybe Positions))) -> Int -> List (Ast Positions) -> List (Ast Positions)
modifyAstStartingAt fn idx inputAsts =
  reverse (updateAsts { res: Nil, posChange: Nothing } inputAsts).res
  where
  go
    :: { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
    -> Ast Positions
    -> { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
  go { res, posChange } ast = case posChange of
    Nothing -> case ast of
      _
        | getStartIdx open == idx ->
            let
              newAst = fn ast
              printed = printPositionedMb newAst
              startIdx = getStartIdx open
              positioned =
                parse printed
                  # hush
                  # fromMaybe (pure ast)
                  <#> map \{ start, end, str } ->
                    { end: startIdx + end
                    , start: startIdx + start
                    , str
                    }
            in
              { res: positioned <> res
              , posChange: Just
                  { old: pick open
                  , new:
                      { start: open.start
                      , end: maybe open.start (getPos >>> _.open.end) $ List.last positioned
                      }
                  }
              }
      Each v inner open close ->
        { res: Each v (reverse innerRes.res) open close' : res
        , posChange: innerRes.posChange
        }
        where
        innerRes = updateAsts { res: Nil, posChange } inner
        close' = case innerRes.posChange of
          Nothing -> close
          Just pc -> updateAstPosition pc close

      With v inner open close ->
        { res: With v (reverse innerRes.res) open close' : res
        , posChange: innerRes.posChange
        }
        where
        innerRes = updateAsts { res: Nil, posChange } inner
        close' = case innerRes.posChange of
          Nothing -> close
          Just pc -> updateAstPosition pc close

      Var _ _ -> doNothing
      Text _ _ -> doNothing

    Just { old, new } ->
      { res: map (updateAstPosition { old, new }) ast : res
      , posChange
      }

    where
    { open } = getPos ast
    doNothing =
      { res: ast : res
      , posChange
      }

  updateAsts
    :: { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
    -> List (Ast Positions)
    -> { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
  updateAsts input asts' = foldl go input asts'

modifyTextAt
  :: (String -> Positions -> (List (Ast Positions)))
  -> Int
  -> List (Ast Positions)
  -> Maybe (List (Ast Positions))
modifyTextAt fn idx inputAsts = Just $
  reverse (updateAsts { res: Nil, posChange: Nothing } inputAsts).res
  where
  go
    :: { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
    -> Ast Positions
    -> { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
  go { res, posChange } ast = case posChange of
    Nothing -> case ast of
      Text text
        pos@
          { start
          , end
          }
        | idx >= start && idx <= end ->
            let
              inserted = fn text pos
              newText = printUnpositioned inserted
            in
              { res: inserted <> res
              , posChange:
                  Just
                    { old: pick pos
                    , new:
                        { start: start
                        , end: end + (String.length newText - String.length text)
                        }
                    }
              }
        | true -> doNothing
      Each v inner open close ->
        { res: Each v (reverse innerRes.res) open close' : res
        , posChange: innerRes.posChange
        }
        where
        innerRes = updateAsts { res: Nil, posChange } inner
        close' = case innerRes.posChange of
          Nothing -> close
          Just pc -> updateAstPosition pc close

      With v inner open close ->
        { res: With v (reverse innerRes.res) open close' : res
        , posChange: innerRes.posChange
        }
        where
        innerRes = updateAsts { res: Nil, posChange } inner
        close' = case innerRes.posChange of
          Nothing -> close
          Just pc -> updateAstPosition pc close

      Var _ _ -> doNothing

    Just { old, new } ->
      { res: map (updateAstPosition { old, new }) ast : res
      , posChange
      }
    where
    doNothing =
      { res: ast : res
      , posChange
      }

  updateAsts
    :: { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
    -> List (Ast Positions)
    -> { posChange :: Maybe PosChange
       , res :: List (Ast Positions)
       }
  updateAsts input = foldl go input

type PosChange = { old :: { start :: Int, end :: Int }, new :: { start :: Int, end :: Int } }

updateAstPosition :: PosChange -> Positions -> Positions
updateAstPosition { old, new } =
  ( updateIndex
  )
  where
  oldStart = old.start
  newEnd = new.end
  oldEnd = old.end

  updateIndex =
    case _ of
      positions@{ start, end, str }
        | old == pick positions -> merge { str } new
        | start >= oldStart ->
            { start: start + (newEnd - oldEnd)
            , end: end + (newEnd - oldEnd)
            , str
            }
        | true -> positions

intersperseEmptyText :: List (Ast Positions) -> List (Ast Positions)
intersperseEmptyText = case _ of
  Nil -> Nil
  Cons ast@(Text _ _) rest -> Cons ast (intersperseEmptyText rest)
  Cons last Nil -> nilText open : last : nilText { start: end, end, str: "" } : Nil
    where
    { open } = getPos last
    end = getEndPosition last
  Cons ast@(Var _ open) rest -> nilText open : ast : (intersperseEmptyText rest)
  Cons (Each p inner open close) rest -> nilText open : (Each p (intersperseEmptyText inner) open close) : (intersperseEmptyText rest)
  Cons (With p inner open close) rest -> nilText open : (With p (intersperseEmptyText inner) open close) : (intersperseEmptyText rest)

  where
  emptyPos { start } = { start, end: start, str: "" }

  nilText open = Text "" (emptyPos open)

filterEmptyText :: List (Ast Positions) -> List (Ast Positions)
filterEmptyText = case _ of
  Nil -> Nil
  Cons (Text "" _) rest -> filterEmptyText rest
  Cons (Each p inner open close) rest -> (Each p (filterEmptyText inner) open close) : filterEmptyText rest
  Cons (With p inner open close) rest -> (With p (filterEmptyText inner) open close) : filterEmptyText rest
  Cons ast rest -> ast : filterEmptyText rest

getEndPosition :: Ast Positions -> Int
getEndPosition ast = maybe open.end _.end close
  where
  { open, close } = getPos ast
