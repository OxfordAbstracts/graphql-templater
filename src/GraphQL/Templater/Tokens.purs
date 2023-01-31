module GraphQL.Templater.Tokens where

import Prelude

openVar = "{{" :: String

closeVar = "}}" :: String

parent = "*parent" :: String

root = "*root" :: String

eachOpen :: String
eachOpen = openVar <> "#each "

eachClose :: String
eachClose = openVar <> "/each" <> closeVar

withOpen :: String
withOpen = openVar <> "#with "

withClose :: String
withClose = openVar <> "/with" <> closeVar

