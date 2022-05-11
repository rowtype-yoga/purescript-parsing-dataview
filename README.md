# purescript-parsing-dataview

[![CI](https://github.com/jamesdbrock/purescript-parsing-dataview/workflows/CI/badge.svg?branch=master)](https://github.com/jamesdbrock/purescript-parsing-dataview/actions)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-parsing-dataview/badge)](http://pursuit.purescript.org/packages/purescript-parsing-dataview/)

Primitive parsers for
`DataView`s on JavaScript `ArrayBuffer`s with the package
[__parsing__](https://pursuit.purescript.org/packages/purescript-parsing/).

With this package, the input stream support of __parsing__
is similar to the built-in stream support of [__Megaparsec__](https://hackage.haskell.org/package/megaparsec):

| Stream type | parsing | Megaparsec |
|----|-----|----|
| UTF-16 strings | String | Text < v2.0|
| UTF-8 strings | | Text ≥ v2.0 |
| Listy strings | Token | String |
| Binary blobs | __DataView__ | ByteString |

## Usage examples

Parse values out of a `dataview :: Data.ArrayBuffer.Types.DataView`. All
`DataView` parsing must be done in an `Effect` context. The `result` will be
`Either` a parse error or the parsed value.

### Parse two numbers

Parse two big-endian IEEE 754 double-precision `Number`s.

```purescript
import Text.Parsing.Parser (runParserT)
import Text.Parsing.Parser.DataView (anyFloat64be)

do
  result <- runParserT dataview do
    float1 <- anyFloat64be
    float2 <- anyFloat64be
    pure $ Tuple float1 float2
```

### Parse an array

Parse an array of `n` 32-bit big-endian signed `Int`s.

```purescript
import Text.Parsing.Parser (runParserT)
import Text.Parsing.Parser.DataView (anyUint32be)
import Data.Unfoldable (replicateA)

do
  result <- runParserT dataview $ replicateA n anyInt32be
```

### Parse UTF-8

Parse a UTF-8 `String` with a length prefix.

We give this as an example, rather than supporting it in the library, because
it depends on
[`Data.TextDecoding.decodeUtf8`](https://pursuit.purescript.org/packages/purescript-text-encoding/docs/Data.TextDecoding#v:decodeUtf8).

```purescript
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (DataView, Uint8Array)
import Data.ArrayBuffer.DataView (buffer, byteOffset, byteLength)
import Data.ArrayBuffer.Typed (part)
import Effect (Effect, liftEffect)
import Text.Parsing.Parser (runParserT, fail)
import Text.Parsing.Parser.DataView (anyUint32be, takeN)
import Data.UInt (toInt)
import Data.Text.Decoding (decodeUtf8)


-- Make a `Uint8Array` Typed Array from a `DataView`. We can do this
-- for the case of `Uint8` because byte arrays are always aligned.
mkUint8Array :: DataView -> Effect Uint8Array
mkUint8Array dv = part (buffer dv) (byteOffset dv) (byteLength dv)

do
  result <- runParserT dataview do
    -- Parse a 32-bit big-endian length prefix for the length
    -- of the UTF-8 string in bytes.
    length      <- anyUint32be
    stringview  <- takeN $ toInt length
    stringarray <- lift $ liftEffect $ mkUint8Array stringview
    case decodeUtf8 stringarray of
      Left err -> fail $ show err
      Right s  -> pure s
```

You might also consider using [__web-encoding__](https://pursuit.purescript.org/packages/purescript-web-encoding) for UTF-8. It will work in *Node.js ≥ v11*.

## Serialization

This package is for reading (`DataView`s on) `ArrayBuffer`s, not writing
them. See the package
[__arraybuffer-builder__](https://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
for a way to
serialize and build `ArrayBuffer`s.


## References

* [MDN `ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
* [MDN `DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView)

## Development

Run the tests with the development `spago` file:

```
spago -x spago-dev.dhall test
```
