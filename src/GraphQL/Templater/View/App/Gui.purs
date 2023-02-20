module GraphQL.Templater.View.App.Gui where

import Prelude

import Data.Bifunctor (lmap)
import Data.Lazy (force)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Templater.Ast (Ast(..))
import GraphQL.Templater.Ast.Suggest (getAstAt, getTypeMapAt)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), getTypeMapFromTree)
import GraphQL.Templater.View.App.Types (Action(..), State)
import GraphQL.Templater.View.Component.Editor as Editor
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
           ( "Editor" :: H.Slot Editor.Query Editor.Output Unit
           , insert_each :: H.Slot q1 (Array String) Unit
           , insert_variable :: H.Slot q2 (Array String) Unit
           , insert_with :: H.Slot q3 (Array String) Unit
           | r
           )
           m
           Action
       )
       Action
gui state =
  HH.div
    [ css "w-[16rem] m-2 p-2 rounded-md border border-gray-400"
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
                  found -> [ HH.text $ "Not in text: " <> show found ]

        _ ->
          [
          ]

  where
  asts = state.ast

  startControls typeTree position =
    [ HH.slot (Proxy :: _ "insert_variable") unit nestedDropdown
        { label: "Insert variable"
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
                        , id: name
                        , selectable: false
                        , children: defer \_ -> getDropdownsFromTypeMap tm'
                        }
                      _ ->
                        NestedDropdown.Node
                          { id: name
                          , label: name
                          }

            in
              getTypeMapAt position asts typeTree
                # fromMaybe Map.empty
                # getDropdownsFromTypeMap
        }
        InsertVariable
    , HH.slot (Proxy :: _ "insert_each") unit nestedDropdown
        { label: "Insert each"
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
                        { id: name
                        , label: name
                        }
                    else []

            in
              getTypeMapAt position asts typeTree
                # fromMaybe Map.empty
                # getDropdownsFromTypeMap
        }
        InsertEach
    , HH.slot (Proxy :: _ "insert_with") unit nestedDropdown
        { label: "Insert with"
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
                        { id: name
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