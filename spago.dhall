{-
-}
{ name = "parsing-dataview"
, dependencies =
  [ "parsing"
  , "arraybuffer-types"
  , "arraybuffer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/jamesdbrock/purescript-parsing-dataview"
}
