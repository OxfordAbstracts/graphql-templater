module GraphQL.Templater.View.App.Gui where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Lazy (force)
import Data.List.NonEmpty ((!!))
import Data.List.NonEmpty as List.NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.String (joinWith)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..), getVartPathPartName)
import GraphQL.Templater.Ast.Print (printVarPartName)
import GraphQL.Templater.Ast.Suggest (getAstAt, getStartIdx, getTypeMapAt)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), getTypeMapFromTree)
import GraphQL.Templater.View.App.Types (Action(..), State)
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
           , edit_variable :: H.Slot q2 (Array (VarPartName Unit)) Unit

           , insert_with :: H.Slot q3 (Array (VarPartName Unit)) Unit
           | r
           )
           m
           Action
       )
       Action
gui state =
  HH.div
    [ css "w-[16rem] min-h-[16rem] m-2 p-2 rounded-md border border-gray-400"
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
                  Var (VarPath varPath _) pos ->
                    [ let
                        path = Array.fromFoldable $ void <<< getVartPathPartName <$> varPath

                        setNewPath selectedPath _ = pure $ Var (VarPath newPath unit) unit
                          where
                          newPath = fromMaybe' (\_ -> void <$> varPath) $ List.NonEmpty.fromFoldable arrPath

                          arrPath = selectedPath # mapWithIndex \pathIdx name ->
                            VarPathPart
                              { name
                              , args: varPath !! pathIdx >>= \(VarPathPart current _) ->
                                  if void current.name == name then
                                    map void <$> current.args
                                  else
                                    Nothing
                              }
                              unit

                      in
                        HH.slot (Proxy :: Proxy "edit_variable") unit nestedDropdown
                          { label: joinWith "." $ map printVarPartName path
                          , path
                          , items: defer \_ ->
                              let
                                getDropdownsFromTypeMap tm = Map.toUnfoldable tm
                                  <#> \(name /\ type_) ->
                                    let
                                      { returns } = force type_
                                    in
                                      case getTypeMapFromTree returns of
                                        Just tm' ->
                                          NestedDropdown.Parent
                                            { label: name
                                            , id: VarPartNameGqlName name unit
                                            , selectable: false
                                            , children: defer \_ -> getDropdownsFromTypeMap tm'
                                            }
                                        _ ->
                                          NestedDropdown.Node
                                            { id: VarPartNameGqlName name unit
                                            , label: name
                                            }

                              in
                                getTypeMapAt position asts typeTree
                                  # fromMaybe Map.empty
                                  # getDropdownsFromTypeMap
                          }
                          (\newPath -> ModifyAstAt (setNewPath newPath) (getStartIdx pos))

                    ]
                  found -> [ HH.text $ "Other ast: " <> show found ]

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
            let
              getDropdownsFromTypeMap tm = Map.toUnfoldable tm
                <#> \(name /\ type_) ->
                  let
                    { returns } = force type_
                  in
                    case getTypeMapFromTree returns of
                      Just tm' -> NestedDropdown.Parent
                        { label: name
                        , id: VarPartNameGqlName name unit
                        , selectable: false
                        , children: defer \_ -> getDropdownsFromTypeMap tm'
                        }
                      _ ->
                        NestedDropdown.Node
                          { id: VarPartNameGqlName name unit
                          , label: name
                          }

            in
              getTypeMapAt position asts typeTree
                # fromMaybe Map.empty
                # getDropdownsFromTypeMap
        }
        InsertVariable
    , HH.slot (Proxy :: Proxy "insert_each") unit nestedDropdown
        { label: "Insert each"
        , path: []
        , items: defer \_ ->
            let
              getDropdownsFromTypeMap tm = Map.toUnfoldable tm
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

            in
              getTypeMapAt position asts typeTree
                # fromMaybe Map.empty
                # getDropdownsFromTypeMap
        }
        InsertEach
    , HH.slot (Proxy :: Proxy "insert_with") unit nestedDropdown
        { label: "Insert with"
        , path: []
        , items: defer \_ ->
            let
              getDropdownsFromTypeMap tm = Map.toUnfoldable tm
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

            in
              getTypeMapAt position asts typeTree
                # fromMaybe Map.empty
                # getDropdownsFromTypeMap
        }
        InsertWith
    ]

toObj :: forall k v. Show k => Map k v -> Object v
toObj = showKeys >>> Object.fromFoldableWithIndex

showKeys :: forall k v. Show k => Map k v -> Map String v
showKeys = (Map.toUnfoldable :: _ -> Array _) >>> map (lmap show) >>> Map.fromFoldable