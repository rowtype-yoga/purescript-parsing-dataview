# purescript-parsing-dataview

[![CI](https://github.com/jamesdbrock/purescript-parsing-dataview/workflows/CI/badge.svg?branch=master)](https://github.com/jamesdbrock/purescript-parsing-dataview/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-parsing-dataview/badge)](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

The module `Text.Parsing.Parser.DataView` provides primitive parsers for parsing
DataViews on Javascript ArrayBuffers with the package
[__purescript-parsing__](https://pursuit.purescript.org/packages/purescript-parsing/).

This module brings the input stream support of __purescript-parsing__ to be
able to roughly match the built-in stream support of __Megaparsec__:

| Stream type | purescript-parsing | Megaparsec |
|----|-----|----|
| UTF-16 strings | String | Text |
| Listy strings | Token | String |
| Binary blobs | __Text.Parsing.Parser.DataView__ | Bytestring |

## Usage examples

Parse values out of a `dataview :: Data.ArrayBuffer.Types.DataView`. All
`DataView` parsing must be done in an `Effect` context. The `result` will be
`Either` a parse error or the parsed value.

### Parse two numbers

Parse two big-endian IEEE 754 double-precision floats.

```purescript
do
  result <- Text.Parsing.Parser.runParserT dataview $ do
    float1 <- anyFloat64be
    float2 <- anyFloat64be
    pure $ Tuple float1 float2
```

### Parse an array

Parse `n` 32-bit signed integers.

```purescript
do
  result <- Text.Parsing.Parser.runParserT dataview $ replicateA n anyInt32be
```

### Parse UTF8

Parse a `String` as UTF8 with a length prefix in a
way that's compatible with the
[`Binary.Put.putStringUtf8`](https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html#v:putStringUtf8)
function from the Haskell
[__binary__](https://hackage.haskell.org/package/binary)
library.
We give this as an example, rather than supporting it in the library, because
it depends on
[`Data.TextDecoding.decodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/docs/Data.TextDecoding#v:decodeUtf8).

```purescript
do
  result <- Text.Parsing.Parser.runParserT dataview $ do
    -- Parse a big-endian 64-big length prefix
    _      <- anyUint32be
    length <- anyUint32be
    stringview <- takeN $ UInt.toInt length
    stringarray <- liftEffect $ mkTypedArray stringview
    pure $ Data.TextDecoding.decodeUtf8 stringarray

where
  mkTypedArray :: Data.ArrayBuffer.Types.DataView -> Effect Data.ArrayBuffer.Types.Uint8Array
  mkTypedArray dv = do
    let buffer     = Data.ArrayBuffer.DataView.buffer dv
        byteOffset = Data.ArrayBuffer.DataView.byteOffset dv
        byteLength = Data.ArrayBuffer.DataView.byteLength dv
    pure $ Data.ArrayBuffer.Typed.part buffer byteOffset byteLength
```


