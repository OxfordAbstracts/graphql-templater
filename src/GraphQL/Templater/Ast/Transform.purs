module GraphQL.Templater.Ast.Transform
  ( insertEachOfPathAt
  , insertEmptyEachAt
  , insertEmptyEachAt'
  , insertSingleVarAt
  , insertTextAt
  , insertVarAt'
  , insertVarPathAt
  , insertWithOfPathAt
  , modifyTextAt
  )
  where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..), reverse, (:))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Ast.Print (printUnpositioned)
import GraphQL.Templater.Positions (Positions)
import Parsing (Position(..))

insertTextAt
  :: String
  -> Int
  -> List (Ast Positions)
  -> Maybe (List (Ast Positions))
insertTextAt text idx = modifyTextAt go idx
  where
  go existing positions@{ start: Position start } =
    let
      { before, after } = String.splitAt (idx - start.index) existing
    in
      Text (before <> text <> after) positions : Nil

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

modifyTextAt
  :: (String -> Positions -> (List (Ast Positions)))
  -> Int
  -> List (Ast Positions)
  -> Maybe (List (Ast Positions))
modifyTextAt fn idx inputAsts = posChange <#> \pc -> updateAstPositions pc (reverse res)
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
          { start: Position start
          , end: Position end
          }
        | idx >= start.index && idx <= end.index ->
            let
              inserted = fn text pos
              newText = printUnpositioned inserted
            in
              { res: inserted <> res
              , posChange:
                  Just
                    { old: pos
                    , new:
                        { start: Position start
                        , end: Position
                            { index: end.index + (String.length newText - String.length text)
                            , line: start.line + getNewlines newText
                            , column: end.column + (getColumn newText - getColumn text)
                            --  (Array.length $ Array.takeWhile (not eq '\n') (Array.reverse newChars)) + 1
                            }
                        }
                    }
              }
        | true -> doNothing
      Each v inner open close ->
        { res: Each v (reverse innerRes.res) open close : res
        , posChange: innerRes.posChange
        }
        where
        innerRes = updateAsts { res, posChange } inner

      With v inner open close ->
        { res: With v (reverse innerRes.res) open close : res
        , posChange: innerRes.posChange
        }
        where
        innerRes = updateAsts { res, posChange } inner
      Var _ _ -> doNothing
    _ -> doNothing
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
  updateAsts input asts' = foldl go input asts'

  { res, posChange } = updateAsts
    { posChange: Nothing
    , res: Nil
    }
    inputAsts

getNewlines :: String -> Int
getNewlines = Array.length <<< Array.filter (eq '\n') <<< toCharArray

getColumn :: String -> Int
getColumn = Array.length <<< Array.takeWhile (not eq '\n') <<< Array.reverse <<< toCharArray

type PosChange = { old :: Positions, new :: Positions }

updateAstPositions :: PosChange -> List (Ast Positions) -> List (Ast Positions)
updateAstPositions { old, new } asts = asts <#> map
  ( updateStartColumn
      >>> updateEndColumn
      >>> updateIndexAndLine
  )
  where
  (Position oldStart) = old.start
  (Position newEnd) = new.end
  (Position oldEnd) = old.end

  updateStartColumn = case _ of
    positions@{ start: Position start }
      | start.line == newEnd.line && start.index >= oldEnd.index -> positions
          { start = Position start
              { column = start.column + (newEnd.column - oldEnd.column)
              }
          }
      | true -> positions

  updateEndColumn = case _ of
    positions@{ end: Position end }
      | end.line == newEnd.line && end.index >= oldEnd.index -> positions
          { end = Position end
              { column = end.column + (newEnd.column - oldEnd.column)
              }
          }
      | true -> positions

  updateIndexAndLine = case _ of
    positions@{ start: Position start, end: Position end }
      | old == positions -> new
      | start.index >= oldStart.index ->
          { start: Position start
              { index = start.index + (newEnd.index - oldEnd.index)
              , line = start.line + (newEnd.line - oldEnd.line)
              }
          , end: Position end
              { index = end.index + (newEnd.index - oldEnd.index)
              , line = end.line + (newEnd.line - oldEnd.line)
              }
          }
      | true -> positions
