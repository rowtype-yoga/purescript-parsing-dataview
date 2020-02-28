# purescript-parsing-dataview

[![CI](https://github.com/jamesdbrock/purescript-parsing-dataview/workflows/CI/badge.svg?branch=master)](https://github.com/jamesdbrock/purescript-parsing-dataview/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-parsing-dataview/badge)](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

The module `Text.Parsing.Parser.DataView` provides primitive parsers for parsing
DataViews on Javascript ArrayBuffers with the package
[__purescript-parsing__](https://github.com/purescript-contrib/purescript-parsing).

This module brings __purescript-parsing__ into rough feature parity
with __Megaparsec__:

| Stream type | purescript-parsing | Megaparsec |
|----|-----|----|
| UTF-16 strings | String | Text |
| Listy strings | Token | String |
| Binary blobs | __DataView__ | Bytestring |

## Installation

```
bower install jamesdbrock/purescript-parsing-dataview --save
```

## Documentation

- [See the tests](test/Main.purs) for some example usages.
- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-parsing-dataview).
