module Main (main) where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Error.Class (try)
import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.GraphQL.AST.Print (printAst)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (message)
import Foreign.Object as Object
import GraphQL.Templater.Eval (eval)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)
import GraphQL.Templater.GetSchema (getGqlDoc)
import GraphQL.Templater.Parser (parse)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Parsing (parseErrorMessage)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action
  = Init
  | SetUrl String
  | SetHeaders String
  | SetTemplate String

component :: forall output m q input. MonadAff m => H.Component q input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  initialState _ =
    { url: initialUrl
    , headers: spy "initialHeaders" initialHeaders
    , template: initialQuery
    , result: ""
    , document: Nothing
    , fullQueryCache: Map.empty
    }

  render state =
    HH.div [ css "flex flex-col min-w-[32rem]" ]
      [ HH.input
          [ css "border-2 rounded-md p-1 m-2 "
          , HE.onValueInput SetUrl
          , HP.value state.url
          , HP.placeholder "Enter graphql endpoint URL"
          ]

      , HH.textarea
          [ HP.value state.headers
          , HP.placeholder "Enter headers"
          , HE.onValueInput SetHeaders
          , css "border-2 rounded-md p-1 m-2 h-12 whitespace-pre-wrap"
          ]
      , HH.textarea
          [ HP.value state.template
          , HP.placeholder "Enter graphql template"
          , HE.onValueInput SetTemplate
          , css "border-2 rounded-md p-1 m-2 h-48 whitespace-pre-wrap"
          ]

      , HH.div [] [ HH.text "Result:" ]
      , HH.pre
          [ HP.ref resultLabel
          , css "border-2 rounded-md p-1 m-2 whitespace-pre-wrap"
          ]
          [ HH.text state.result ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2 whitespace-pre" ]
          [ HH.text $ either parseErrorMessage (toGqlString >>> fromMaybe "No query") $ parse state.template
          ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2" ]
          [ HH.text $ either parseErrorMessage (map (map (const unit) >>> show) >>> intercalate "\n") $ parse state.template
          ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2" ]
          [ HH.text $ case state.document of
              Just doc -> doc
              Nothing -> "Loading schema"
          ]
      ]

  handleAction = case _ of
    Init -> loadSchema
    SetUrl url -> do
      H.modify_ _ { url = url }
      loadSchema
    SetHeaders headers -> do
      H.modify_ _ { headers = headers }
    SetTemplate template -> do
      { url, headers } <- H.modify _ { template = template }
      case parse template of
        Left _ -> pure unit
        Right ast -> do
          res <- eval
            { ast
            , url
            , headers: spy "headers" $ headers # split (Pattern "\n") # mapMaybe \str -> case split (Pattern ":") str of
                [ key, value ] -> Just $ RequestHeader key value
                _ -> Nothing
            }
          let
            resultStr = case res of
              Nothing -> "No query"
              Just (Left err) -> show err
              Just (Right query) -> query

          H.modify_ _ { result = resultStr }

    where
    loadSchema = do
      st <- H.get
      doc <- H.liftAff $ try $ getGqlDoc st.url $ Object.fromFoldable
        $ st.headers
            # split (Pattern "\n")
            # mapMaybe \str -> case split (Pattern ":") str of
                [ key, value ] -> Just $ Tuple key value
                _ -> Nothing
      H.modify_ _
        { document = Just $ either
            ( \err ->
                "Failed to load schema: " <> message err
            )
            printAst
            doc
        }

  resultLabel = H.RefLabel "result"

-- where 

css
  :: forall r a
   . String
  -> IProp
       ( class :: String
       | r
       )
       a
css = class_ <<< ClassName

foreign import initialUrl :: String
foreign import initialHeaders :: String
foreign import initialQuery :: String