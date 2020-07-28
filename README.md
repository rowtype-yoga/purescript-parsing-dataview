# purescript-parsing-dataview

[![CI](https://github.com/jamesdbrock/purescript-parsing-dataview/workflows/CI/badge.svg?branch=master)](https://github.com/jamesdbrock/purescript-parsing-dataview/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-parsing-dataview/badge)](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

Primitive parsers for parsing
`DataView`s on Javascript `ArrayBuffer`s with the package
[__purescript-parsing__](https://pursuit.purescript.org/packages/purescript-parsing/).

With this package, the input stream support of __purescript-parsing__
roughly matches the built-in stream support of __Megaparsec__:

| Stream type | purescript-parsing | Megaparsec |
|----|-----|----|
| UTF-16 strings | String | Text |
| Listy strings | Token | String |
| Binary blobs | __DataView__ | ByteString |

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

Parse an array of `n` 32-bit signed integers.

```purescript
do
  result <- Text.Parsing.Parser.runParserT dataview $ replicateA n anyInt32be
```

### Parse UTF8

Parse a `String` as UTF8 with a length prefix.

We give this as an example, rather than supporting it in the library, because
it depends on
[`Data.TextDecoding.decodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/docs/Data.TextDecoding#v:decodeUtf8).

```purescript
mkTypedArray :: Data.ArrayBuffer.Types.DataView -> Effect Data.ArrayBuffer.Types.Uint8Array
mkTypedArray dv = do
  let buffer     = Data.ArrayBuffer.DataView.buffer dv
      byteOffset = Data.ArrayBuffer.DataView.byteOffset dv
      byteLength = Data.ArrayBuffer.DataView.byteLength dv
  Data.ArrayBuffer.Typed.part buffer byteOffset byteLength

do
  result <- Text.Parsing.Parser.runParserT dataview $ do
    -- Parse a 32-bit big-endian length prefix for the length of the utf8 string,
    -- in bytes.
    length      <- anyUint32be
    stringview  <- takeN $ UInt.toInt length
    stringarray <- liftEffect $ mkTypedArray stringview
    case Data.TextDecoding.decodeUtf8 stringarray of
      Left err -> Data.Parsing.Parser.fail $ show err
      Right s  -> pure s
```

## Serialization

This package is for reading (`DataView`s on) `ArrayBuffer`s, not writing
them. See the package
[__purescript-arraybuffer-builder__](https://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
for a way to
serialize and build `ArrayBuffer`s.


## References

* [MDN `ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
* [MDN `DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView)

## Notes

Perhaps some day this package can be
[merged into __purescript-parsing__](https://github.com/purescript-contrib/purescript-parsing/issues/88),
but for now it has too many non-*purescript-contrib* dependencies.
