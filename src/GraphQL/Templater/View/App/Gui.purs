module GraphQL.Templater.View.App.Gui
  ( gui
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Lazy (force)
import Data.List (List(..), fold, head, takeWhile)
import Data.List as List
import Data.List.NonEmpty ((!!))
import Data.List.NonEmpty as List.NonEmpty
import Data.List.Types (toList)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.String (joinWith)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..), getVartPathPartName)
import GraphQL.Templater.Ast.Print (printVarPartName)
import GraphQL.Templater.Ast.Suggest (getAstAt, getJsonPosPathAt, getStartIdx, getTypeMapAt)
import GraphQL.Templater.Ast.Transform (justPos, nothingPos)
import GraphQL.Templater.Ast.TypeCheck (varPathToPosAndArgs)
import GraphQL.Templater.JsonPos (JsonPos(..), getJsonPosArg, normalizePos)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), getTypeMapFromTree)
import GraphQL.Templater.View.App.Types (Action(..), State)
import GraphQL.Templater.View.Component.ArgGui (argGui)
import GraphQL.Templater.View.Component.ArgGui as ArgGui
import GraphQL.Templater.View.Component.NestedDropdown (nestedDropdown)
import GraphQL.Templater.View.Component.NestedDropdown as NestedDropdown
import GraphQL.Templater.View.Html.Utils (css)
import Halogen (defer)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

gui
  :: forall q1 q2 q3 m r
   . MonadEffect m
  => State
  -> HH.HTML
       ( H.ComponentSlot
           ( insert_each :: H.Slot q1 (Array (VarPartName Unit)) Unit
           , insert_variable :: H.Slot q2 (Array (VarPartName Unit)) Unit
           , insert_with :: H.Slot q3 (Array (VarPartName Unit)) Unit
           , edit_variable ::
               H.Slot q2 (Array (VarPartName Unit))
                 { pos :: Positions
                 , vp :: VarPath Positions
                 }
           , edit_each ::
               H.Slot q2 (Array (VarPartName Unit))
                 { pos :: Positions
                 , vp :: VarPath Positions
                 }
           , edit_with ::
               H.Slot q2 (Array (VarPartName Unit))
                 { pos :: Positions
                 , vp :: VarPath Positions
                 }

           , edit_arg ::
               H.Slot q2 ArgGui.Output
                 { pos :: Positions
                 , vp :: VarPath Positions
                 }
           | r
           )
           m
           Action
       )
       Action
gui state =
  HH.div
    [ css "w-[20rem] min-h-[16rem] m-2 p-2 rounded-md border border-gray-400"
    ]
    $
      case state.schemaTypeTree of
        Just typeTree ->
          let
            position = fromMaybe (String.length state.template) state.cursorPosition
          in
            case getAstAt position asts of
              Nothing -> startControls typeTree position
              Just selectedAst ->
                case selectedAst of
                  Text _ _ -> startControls typeTree position
                  Var vp pos ->
                    [ let
                        path = varPathToDropdownPath vp
                        setNewVarPath selectedPath _ = pure $ Var (dropdownPathToVarPath vp selectedPath) (Just pos)
                      in
                        HH.slot (Proxy :: Proxy "edit_variable") { pos, vp } nestedDropdown
                          { label: printPath path
                          , path
                          , items: defer \_ ->
                              getTypeMapAt position asts typeTree
                                # fromMaybe Map.empty
                                # getVariableItems
                          }
                          \selectedPath -> ModifyAstAt (setNewVarPath selectedPath) (getStartIdx pos)

                    , argGui' typeTree position pos vp
                    ]

                  Each vp inner pos close ->
                    [ let
                        path = varPathToDropdownPath vp
                        setNewVarPath selectedPath _ = pure $ Each (dropdownPathToVarPath vp selectedPath) (justPos inner) (Just pos) (Just close)
                      in
                        HH.slot (Proxy :: Proxy "edit_each") { pos, vp } nestedDropdown
                          { label: "#each " <> (printPath path)
                          , path
                          , items: defer \_ ->
                              getTypeMapAt position asts typeTree
                                # fromMaybe Map.empty
                                # getEachItems
                          }
                          \selectedPath -> ModifyAstAt (setNewVarPath selectedPath) (getStartIdx pos)

                    , argGui' typeTree position pos vp
                    ]
                  With vp inner pos close ->
                    [ let
                        path = varPathToDropdownPath vp
                        setNewVarPath selectedPath _ = pure $ With (dropdownPathToVarPath vp selectedPath) (justPos inner) (Just pos) (Just close)
                      in
                        HH.slot (Proxy :: Proxy "edit_with") { pos, vp } nestedDropdown
                          { label: "#with " <> (printPath path)
                          , path
                          , items: defer \_ ->
                              getTypeMapAt position asts typeTree
                                # fromMaybe Map.empty
                                # getWithItems
                          }
                          \selectedPath -> ModifyAstAt (setNewVarPath selectedPath) (getStartIdx pos)

                    , argGui' typeTree position pos vp
                    ]

        _ ->
          [
          ]

  where
  asts = state.ast

  startControls typeTree position =
    [ HH.slot (Proxy :: Proxy "insert_variable") unit nestedDropdown
        { label: "Insert variable"
        , path: []
        , items: defer \_ ->
            getTypeMapAt position asts typeTree
              # fromMaybe Map.empty
              # getVariableItems
        }
        InsertVariable
    , HH.slot (Proxy :: Proxy "insert_each") unit nestedDropdown
        { label: "Insert each"
        , path: []
        , items: defer \_ ->
            getTypeMapAt position asts typeTree
              # fromMaybe Map.empty
              # getEachItems
        }
        InsertEach
    , HH.slot (Proxy :: Proxy "insert_with") unit nestedDropdown
        { label: "Insert with"
        , path: []
        , items: defer \_ ->
            getTypeMapAt position asts typeTree
              # fromMaybe Map.empty
              # getWithItems
        }
        InsertWith
    ]

  getVariableItems tm = Map.toUnfoldable tm
    <#> \(name /\ type_) ->
      let
        { returns } = force type_
      in
        case getTypeMapFromTree returns of
          Just tm' -> NestedDropdown.Parent
            { label: name
            , id: VarPartNameGqlName name unit
            , selectable: false
            , children: defer \_ -> getVariableItems tm'
            }
          _ ->
            NestedDropdown.Node
              { id: VarPartNameGqlName name unit
              , label: name
              }

  getEachItems tm = Map.toUnfoldable tm
    >>= \(name /\ type_) ->
      let
        { returns } = force type_

        isList = isList' returns

        isList' t = case t of
          ListType _ -> true
          NonNull t' -> isList' t'
          _ -> false
      in
        if isList then pure $
          NestedDropdown.Node
            { id: VarPartNameGqlName name unit
            , label: name
            }
        else []

  getWithItems tm = Map.toUnfoldable tm
    >>= \(name /\ type_) ->
      let
        { returns } = force type_

        isObject = isObject' returns

        isObject' t = case t of
          ObjectType _ -> true
          NonNull t' -> isObject' t'
          _ -> false
      in
        if isObject then pure $
          NestedDropdown.Node
            { id: VarPartNameGqlName name unit
            , label: name
            }
        else []

  argGui' :: GqlTypeTree -> Int -> Positions -> VarPath Positions -> HH.HTML _ _
  argGui' typeTree position pos vp@(VarPath varPath _) =
    let
      contextPos =
        (map { pos: _, args: Nothing } <<< Pos) <$> (getJsonPosPathAt position asts)

      varPathAtPosition = toList varPath # takeWhile \(VarPathPart _ p) -> p.start <= position

      jsonPos = varPathToPosAndArgs $ Array.fromFoldable varPathAtPosition

      path =
        map (map void <<< fold <<< _.args) <$>
          (normalizePos $ jsonPos <> contextPos)
    in
      HH.slot (Proxy :: Proxy "edit_arg") { pos, vp } argGui
        { arguments: head jsonPos # maybe Nil (getJsonPosArg >>> _.args >>> maybe Nil (map void))
        , path
        , typeTree
        }
        (HandleArgGuiOutput { startIdx: pos.start, pathIdx: List.length varPathAtPosition  - 1})

dropdownPathToVarPath :: VarPath Positions -> Array (VarPartName Unit) -> VarPath (Maybe Positions)
dropdownPathToVarPath (VarPath varPath _p) selectedPath = (VarPath newPath Nothing)
  where
  newPath = fromMaybe' (\_ -> justPos varPath) $ List.NonEmpty.fromFoldable arrPath

  arrPath :: Array (VarPathPart (Maybe Positions))
  arrPath = selectedPath # mapWithIndex \pathIdx name_ ->
    let
      name = nothingPos name_
    in
      VarPathPart
        { name
        , args: varPath !! pathIdx >>= \(VarPathPart current _) ->
            if void current.name == void name then
              justPos <$> current.args
            else
              Nothing
        }
        Nothing

varPathToDropdownPath :: forall p. VarPath p -> Array (VarPartName Unit)
varPathToDropdownPath (VarPath varPath _) = Array.fromFoldable $ void <<< getVartPathPartName <$> varPath

printPath :: forall a. Array (VarPartName a) -> String
printPath path = joinWith "." $ map printVarPartName path