{-
-}
{ name = "parsing-dataview"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-types"
  , "effect"
  , "float32"
  , "maybe"
  , "parsing"
  , "prelude"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-2-Clause"
, repository = "https://github.com/jamesdbrock/purescript-parsing-dataview"
}
