module GraphQL.Templater.MakeQuery where

import Prelude

import Data.Foldable (class Foldable, fold, foldl, lookup)
import Data.GraphQL.AST (Argument(..), Arguments(..), ExecutableDefinition, Field(..), SelectionSet(..))
import Data.List (List(..), reverse, tail, (:))
import Data.List.NonEmpty (NonEmptyList, head, uncons)
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map, unionWith)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (none)
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Positions (Positions)
import Unsafe.Coerce (unsafeCoerce)

data SelectionTree =
  SelectionTree Selections

type Selections = Map String (List (Tuple Arguments SelectionTree))

buildSelectionTree :: forall a. List (Ast a) -> Selections
buildSelectionTree = go Nil Map.empty
  where
  go :: List (Tuple Arguments String) -> _ -> List (Ast a) -> Selections
  go ancestors = foldl (step ancestors)

  step :: List (Tuple Arguments String) -> Selections -> (Ast a) -> Selections
  step ancestors res = case _ of
    Var (VarPath v _) _ -> normalizeAndInsertPath ancestors res v
    Each (VarPath v _) ast _ ->
      go (normalizePath ancestors (pure $ NonEmpty.head v))
        (normalizeAndInsertPath ancestors res v)
        ast
    Text _ _ -> res

  normalizeAndInsertPath ancestors res v =
    let
      path = normalizePath ancestors (NonEmpty.toList v)
    in
      insertPath res path

  insertPath :: Selections -> List (Tuple Arguments String) -> Selections
  insertPath sels = case _ of
    Nil -> sels
    Tuple args name : rest ->
      let
        handleVals :: List (Tuple Arguments SelectionTree) -> List (Tuple Arguments SelectionTree)
        handleVals vals = case lookup args vals of
          Just (SelectionTree tree) -> vals <#> \(Tuple a v') ->
            if a == args then
              Tuple a (SelectionTree $ insertPath tree rest)
            else
              Tuple a v'

          _ -> Tuple args (SelectionTree Map.empty) : vals
      in
        Map.alter (Just <<< handleVals <<< fromMaybe Nil) name sels

  normalizePath :: List (Tuple Arguments String) -> List (VarPathPart a) -> List (Tuple Arguments String)
  normalizePath res = case _ of
    VarPathPart { name: (VarPartNameRoot _) } _ : rest -> normalizePath Nil rest
    VarPathPart { name: (VarPartNameParent _) } _ : rest -> normalizePath (fromMaybe Nil $ tail res) rest
    VarPathPart { name: (VarPartNameGqlName gqlName _), args } _ : rest ->
      normalizePath (Tuple (getPartArgs args) gqlName : res) rest
    _ -> res

  -- getPartArgs :: VarPathPart _ -> Arguments
  getPartArgs = maybe nilArgs fst

  nilArgs = Arguments Nil

-- getPath ancestors = foldl (\acc (VarPathPart { name } _) -> acc <> "." <> name) ancestors.name root.selectionSet

-- getSelectionSet :: List AstPos -> SelectionSet
-- getSelectionSet = 

-- mergeGqlFields :: List Field -> List Field -> List Field
-- mergeGqlFields fs1 fs2

-- placeInTree
--   :: forall a
--    . Map (VarPartName a) (Selection (Map (VarPartName a)) (VarPartName a))
--   -> Map (VarPartName a) (Selection (Map (VarPartName a)) (VarPartName a))
-- placeInTree = unsafeCoerce

-- unifySelection
--   :: forall n sel
--    . Ord n
--   => Foldable sel
--   => sel (Selection sel n)
--   -> Map n (Selection (Map n) n)
-- unifySelection = foldl resolve Map.empty
--   where
--   resolve :: Map n (Selection (Map n) n) -> Selection sel n -> Map n (Selection (Map n) n)
--   resolve res (Selection sel@{ name }) = Map.alter alter name res
--     where
--     alter = case _ of
--       Just (Selection found) -> Just $ Selection $ found
--         { selectionSet = unionWith mergeSelections found.selectionSet (unifySelection sel.selectionSet)
--         }

--       Nothing -> Just $ Selection $ sel { selectionSet = unifySelection sel.selectionSet }

--   mergeSelections :: Selection (Map n) n -> Selection (Map n) n -> Selection (Map n) n
--   mergeSelections (Selection sel1) (Selection sel2) = Selection sel1
--     { selectionSet =
--         unionWith mergeSelections sel1.selectionSet sel2.selectionSet
--     }

-- resolveAncestors :: forall a. Selection List (VarPartName a) -> Selection List String
-- resolveAncestors selection' = go { root: selection', ancestors: pure selection' } selection'
--   where
--   go
--     :: { root :: Selection List (VarPartName a), ancestors :: List (Selection List (VarPartName a)) }
--     -> Selection List (VarPartName a)
--     -> (Selection List String)
--   go { root, ancestors } (Selection sel@{name}) = Selection sel
--     { name = ?d name
--     -- foldl (\acc (VarPathPart { name } _) -> acc <> "." <> name) ancestors.name root.selectionSet
--     , selectionSet = map (go { root, ancestors: Selection sel : ancestors }) sel.selectionSet
--     }

-- -- List (Selection Maybe n) -> Map n (Selection Maybe n)

-- getSelection :: forall a. Ast a -> List (Selection List (VarPartName a))
-- getSelection = case _ of
--   Var v _ -> pure $ getVarPathSelection v
--   Each v ast _ -> getVarPathSelection v : (getSelection =<< ast)
--   Text _ _ -> Nil

-- getVarPathSelection :: forall a. VarPath a -> Selection List (VarPartName a)
-- getVarPathSelection (VarPath p _) = getVarPathPartsSelection p

-- getVarPathPartsSelection :: forall a. NonEmptyList (VarPathPart a) -> Selection List (VarPartName a)
-- getVarPathPartsSelection p = Selection
--   { name: part.name
--   , arguments: maybe Nil (fst >>> unwrap) part.args
--   , selectionSet: case NonEmpty.fromList tail of
--       Nothing -> none
--       Just t -> pure $ getVarPathPartsSelection t
--   }
--   where
--   { head: VarPathPart part _, tail } = uncons p

-- data Selection sel n = Selection
--   { name :: n
--   , arguments :: List Argument
--   , selectionSet :: sel (Selection sel n)
--   }

-- x :: forall a. List a -> Maybe (NonEmptyList a) 
-- x = ?d