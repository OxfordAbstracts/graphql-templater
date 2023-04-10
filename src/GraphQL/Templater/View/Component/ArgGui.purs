module GraphQL.Templater.View.Component.ArgGui
  ( argGui
  , Input
  , Output(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either, note)
import Data.GraphQL.AST (ArgumentsDefinition(..), Document(..), InputValueDefinition(..), ListType(..), NamedType(..), NonNullType(..), T_InputValueDefinition, Type(..))
import Data.GraphQL.AST as AST
import Data.Int as Int
import Data.Lens (class Wander, prism', toListOf, traversed)
import Data.List (List(..), any, findMap, fold, intercalate)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (Tuple(..), uncurry)
import Debug (spy)
import GraphQL.Templater.Ast (Args)
import GraphQL.Templater.Ast.Argument (ArgName(..), Argument(..), NullValue(..), StringValue(..), Value(..))
import GraphQL.Templater.Ast.Argument as AstArg
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..))
import GraphQL.Templater.TypeDefs (GqlTypeTree, getArgsAtPath, getTypeAtPath)
import GraphQL.Templater.View.Html.Input (input, intInput, numberInput)
import GraphQL.Templater.View.Html.Utils (css)
import Halogen as H
import Halogen.HTML as HH

data Action
  = Init
  | Receive Input
  | SetArgValue InputValueDefinition String
      ( Either String (Unit -> Value Unit)
      )

type Input =
  { path :: List (NormalizedJsonPos (Args Unit))
  , arguments :: Args Unit
  , typeTree :: GqlTypeTree
  }

type State =
  { input :: Input
  , invalidArg :: Maybe { valDef :: InputValueDefinition, val :: String }
  }

data Output = NewArgs (Args Unit)

argGui :: forall m q. H.Component q Input Output m
argGui =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  initialState :: Input -> State
  initialState =
    { input: _
    , invalidArg: Nothing
    }

  render st = HH.div
    [ css "p-4" ]
    [ HH.div
        []
        [ renderArguments st ]
    ]

  renderArguments :: State -> HH.HTML _ Action
  renderArguments _state@{ input } = either nullMsg identity do
    (ArgumentsDefinition argDefs) <- note ("No args available for " <> intercalate "." strPath) $ getArgsAtPath strPath input.typeTree
    pure $ HH.div_
      [ HH.div [ css "text-sm" ] [ HH.text $ "Args for " <> intercalate "." strPath ]
      , HH.div [] $ Array.fromFoldable argDefs <#> renderInputValueDefinition input.typeTree input.arguments
      ]

    where
    strPath = input.path >>= case _ of
      Key { name } _ -> pure name
      Index _ _ -> Nil

    nullMsg str = HH.div [ css "text-gray-400 text-sm" ] [ HH.text str ]

  renderInputValueDefinition :: GqlTypeTree -> Args Unit -> InputValueDefinition -> HH.HTML _ Action
  renderInputValueDefinition tree args (InputValueDefinition ivd) =
    case ivd.type of
      Type_ListType _ -> HH.text ""
      _ -> case getInputValueDefinitionTypeName ivd of
        "String" -> HH.div [ css "pt-3" ]
          [ input
              { label: ivd.name
              , value: fold $ getValueInputString =<< argVal
              , placeholder: ""
              , onInput: \str ->
                  SetArgValue (InputValueDefinition ivd) str
                    $ Right
                    $ Value_StringValue (StringValue str)
              }

          ]
        "Int" -> HH.div [ css "pt-3" ]
          [ intInput
              { label: ivd.name
              , value: Int.fromString =<< getValueInputString =<< argVal
              , onInput: \str intM ->
                  SetArgValue (InputValueDefinition ivd) str
                    $ intM
                        # note "Not an integer"
                        <#> \int ->
                          Value_IntValue (AstArg.IntValue int)
              }
          ]
        "Float" -> HH.div [ css "pt-3" ]
          [ numberInput
              { label: ivd.name
              , value: Number.fromString =<< getValueInputString =<< argVal
              , onInput: \str numberM ->
                  SetArgValue (InputValueDefinition ivd) str
                    $ numberM
                        # note "Not a number"
                        <#> \num ->
                          Value_FloatValue (AstArg.FloatValue num)
              }
          ]
        _ -> case getTypeAtPath (pure $ getInputValueDefinitionTypeName ivd) tree of
          _ -> HH.text ""
    where
    argVal :: Maybe (Value Unit)
    argVal = args # findMap \(Argument a@{ name: ArgName name _ }) ->
      if name == ivd.name then
        Just (void a.value)
      else
        Nothing

  handleAction :: Action -> H.HalogenM State Action _ _ m Unit
  handleAction = case _ of
    Init -> pure unit

    Receive input ->
      H.modify_ _ { input = input }

    SetArgValue (InputValueDefinition ivd) str valE -> do
      st <- H.modify \state@{ input } ->
        let
          updateArg = \(Argument a@{ name: ArgName name _ }) ->
            Argument case name == ivd.name, valE of
              true, Right val -> a { value = val unit }
              _, _ -> a
        in
          state
            { input = input
                { arguments =
                    if either (\_ -> str == "") (const false) valE then
                      input.arguments # List.filter \(Argument { name: ArgName name _ }) ->
                        name /= ivd.name
                    else
                      appendArgIfNotThere ivd input.arguments <#> updateArg
                }
            , invalidArg = case valE of
                Left val -> Just { valDef: InputValueDefinition ivd, val }
                _ -> Nothing
            }

      H.raise $ NewArgs st.input.arguments

appendArgIfNotThere :: T_InputValueDefinition -> Args Unit -> Args Unit
appendArgIfNotThere ivd args =
  if any sameArgName args then
    args
  else
    args <> pure
      ( Argument
          { name: ArgName ivd.name unit
          , value: Value_NullValue NullValue unit
          , pos: unit
          }
      )

  where
  sameArgName (Argument { name: ArgName name _ }) = name == ivd.name

getInputValueDefinitionTypeName :: T_InputValueDefinition -> String
getInputValueDefinitionTypeName ivd = go ivd.type
  where
  go = case _ of
    Type_NamedType (NamedType name) -> name
    Type_ListType (ListType t) -> go t
    Type_NonNullType (NonNullType_ListType (ListType t)) -> go t
    Type_NonNullType (NonNullType_NamedType t) -> go $ Type_NamedType t

getArgInput :: Document -> AST.Type -> Maybe ArgInput
getArgInput (Document defs) = case _ of
  Type_NamedType (NamedType name) -> do
    Nothing
  _ -> Nothing

data ArgInput
  = AiObject
  | AiText
  | AiInt
  | AiNumber
  | AiBoolean
  | AiEnum
  | AiList

getArgLookups :: Document -> { inputObjects :: Map String AST.InputObjectTypeDefinition }
getArgLookups doc =
  { inputObjects: doc # getInputObjects # map getNamedType # Map.fromFoldableWith const
  }
  where
  getNamedType iotd@(AST.InputObjectTypeDefinition { name }) = Tuple name iotd

getInputObjects ∷ Document → List AST.InputObjectTypeDefinition
getInputObjects = toListOf inputObjectTypeDefinitionLens

inputObjectTypeDefinitionLens :: forall c. Choice c => Wander c => c AST.InputObjectTypeDefinition AST.InputObjectTypeDefinition -> c AST.Document AST.Document
inputObjectTypeDefinitionLens = uPrism AST._Document
  <<< traversed
  <<< uPrism AST._Definition_TypeSystemDefinition
  <<< uPrism AST._TypeSystemDefinition_TypeDefinition
  <<< uPrism AST._TypeDefinition_InputObjectTypeDefinition

uPrism :: forall s a c. Tuple (a -> s) (s -> Maybe a) -> (Choice c => c a a -> c s s)
uPrism = uncurry prism'

getValueInputString :: forall a. Value a -> Maybe String
getValueInputString = case _ of
  Value_StringValue (AstArg.StringValue s) _ -> Just s
  Value_IntValue (AstArg.IntValue s) _ -> Just $ show s
  Value_FloatValue (AstArg.FloatValue s) _ -> Just $ show s
  Value_BooleanValue (AstArg.BooleanValue s) _ -> Just $ show s
  Value_EnumValue (AstArg.EnumValue s) _ -> Just $ show s
  _ -> Nothing
