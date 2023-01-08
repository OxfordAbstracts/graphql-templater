{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "affjax-node"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "graphql-parser"
  , "halogen"
  , "http-methods"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "record"
  , "record-extra"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "transformers"
  , "tuples"
  , "unicode"
  , "unordered-collections"
  , "unsafe-coerce"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
