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
, license = "MIT"
, repository = "https://github.com/jamesdbrock/purescript-parsing-dataview"
}
