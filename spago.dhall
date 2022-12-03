{-
-}
{ name = "parsing-dataview"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-types"
  , "effect"
  , "enums"
  , "float32"
  , "maybe"
  , "parsing"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-2-Clause"
, repository = "https://github.com/rowtype-yoga/purescript-parsing-dataview"
}
