-- | Primitive parsers for input type `DataView`.
-- |
-- | All of these primitive parsers will consume when they succeed.
-- |
-- | All of these primitive parsers will not consume and will automatically
-- | backtrack when they fail.
-- |
-- | See the package README for usage examples.
-- |
-- | ### Mutable ArrayBuffer
-- |
-- | All of the parsers in this module operate on an input stream of
-- | [__Data.ArrayBuffer.Types.DataView__](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:DataView),
-- | which represents a range of a mutable
-- | [__Data.ArrayBuffer.Types.ArrayBuffer__](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer).
-- |
-- | For operations for working with `DataView`, see
-- | module
-- | [__Data.ArrayBuffer.DataView__](https://pursuit.purescript.org/packages/purescript-arraybuffer/docs/Data.ArrayBuffer.DataView)
-- | in package __arraybuffer__.
-- |
-- | Reading from an `ArrayBuffer` is an `Effect`ful activity, so
-- | all parsers in this module must be run in a
-- | `MonadEffect m => ParserT DataView m` context with
-- | `Parsing.runParserT`.
-- |
-- | ### Position
-- |
-- | In a `DataView` parser, the `Position {index}` counts the number of
-- | bytes since the beginning of the input.
-- |
-- | The `Postion {line,column}` fields are unused and will remain constant *1*.
module Parsing.DataView
  ( takeN
  , takeRest
  , eof
  , match
  , anyTill
  , anyPrim
  , anyInt8
  , anyInt16be
  , anyInt16le
  , anyInt32be
  , anyInt32le
  , anyUint8
  , anyUint16be
  , anyUint16le
  , anyUint32be
  , anyUint32le
  , anyFloat32be
  , anyFloat32le
  , anyFloat64be
  , anyFloat64le
  , satisfy
  , satisfyInt8
  , satisfyInt16be
  , satisfyInt16le
  , satisfyInt32be
  , satisfyInt32le
  , satisfyUint8
  , satisfyUint16be
  , satisfyUint16le
  , satisfyUint32be
  , satisfyUint32le
  , satisfyFloat32be
  , satisfyFloat32le
  , satisfyFloat64be
  , satisfyFloat64le
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.DataView (Endian(LE, BE))
import Data.ArrayBuffer.DataView (buffer, byteLength, byteOffset, get, part) as DV
import Data.ArrayBuffer.Types (DataView, Int16, Int32, Int8, Uint16, Uint32, Uint8, Float32, Float64, ByteLength)
import Data.ArrayBuffer.ValueMapping (class BytesPerType, class BinaryValue, class ShowArrayViewType, byteWidth)
import Data.Float32 as Float32
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Effect.Class (class MonadEffect, liftEffect)
import Parsing (ParseState(..), ParserT, Position(..), consume, fail, getParserT, stateParserT)
import Parsing.Combinators (alt, try, tryRethrow)
import Type.Proxy (Proxy(..))

-- | Parse one fixed-bit-width `Data.ArrayBuffer.Types.ArrayViewType` primitive
-- | of a given endianness.
-- |
-- | #### Example
-- |
-- | Parse a little-endian 32-bit two’s-complement signed integer (4 bytes):
-- |
-- |     anyPrim LE (Proxy :: Proxy Int32)
-- |
-- | or just use the convenience function `anyInt32le`, see below.
anyPrim
  :: forall a name m t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => MonadEffect m
  => Endian
  -> Proxy a
  -> ParserT DataView m t
anyPrim endian _ = do
  ParseState input (Position { index }) _ <- getParserT
  lift (liftEffect (DV.get endian (Proxy :: Proxy a) input index)) >>= case _ of
    Nothing -> fail "anyPrim unexpected end of DataView"
    Just i -> do
      stateParserT \_ -> Tuple i $ ParseState input (Position { line: 1, column: 1, index: index + byteWidth (Proxy :: Proxy a) }) true

-- | Parse one 8-bit two’s-complement signed integer (byte).
anyInt8 :: forall m. MonadEffect m => ParserT DataView m Int
anyInt8 = anyPrim LE (Proxy :: Proxy Int8)

-- | Parse one 16-bit big-endian two’s-complement signed integer.
anyInt16be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16be = anyPrim BE (Proxy :: Proxy Int16)

-- | Parse one 16-bit little-endian two’s-complement signed integer.
anyInt16le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16le = anyPrim LE (Proxy :: Proxy Int16)

-- | Parse one 32-bit big-endian two’s-complement signed integer.
anyInt32be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32be = anyPrim BE (Proxy :: Proxy Int32)

-- | Parse one 32-bit little-endian two’s-complement signed integer.
anyInt32le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32le = anyPrim LE (Proxy :: Proxy Int32)

-- | Parse one 8-bit unsigned integer (octet).
anyUint8 :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint8 = anyPrim LE (Proxy :: Proxy Uint8)

-- | Parse one 16-bit big-endian unsigned integer.
anyUint16be :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint16be = anyPrim BE (Proxy :: Proxy Uint16)

-- | Parse one 16-bit little-endian unsigned integer.
anyUint16le :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint16le = anyPrim LE (Proxy :: Proxy Uint16)

-- | Parse one 32-bit big-endian unsigned integer.
anyUint32be :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint32be = anyPrim BE (Proxy :: Proxy Uint32)

-- | Parse one 32-bit little-endian unsigned integer.
anyUint32le :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint32le = anyPrim LE (Proxy :: Proxy Uint32)

-- | Parse one 32-bit big-endian IEEE 754 floating-point number.
anyFloat32be :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32be = anyPrim BE (Proxy :: Proxy Float32)

-- | Parse one 32-bit little-endian IEEE 754 floating-point number.
anyFloat32le :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32le = anyPrim LE (Proxy :: Proxy Float32)

-- | Parse one 64-bit big-endian IEEE 754 floating-point number.
anyFloat64be :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64be = anyPrim BE (Proxy :: Proxy Float64)

-- | Parse one 64-bit little-endian IEEE 754 floating-point number.
anyFloat64le :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64le = anyPrim LE (Proxy :: Proxy Float64)

-- | Parse one fixed-bit-width primitive that satisfies the given predicate.
-- |
-- | #### Example
-- |
-- | Parse a little-endian 32-bit signed integer that is equal to *3*:
-- |
-- |     satisfy LE (Proxy :: Proxy Int32) (_ == 3)
-- |
-- | or just use the convenience function `satisfyInt32le`, see below.
satisfy
  :: forall a name m t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Show t
  => MonadEffect m
  => Endian
  -> Proxy a
  -> (t -> Boolean)
  -> ParserT DataView m t
satisfy endian proxy f = tryRethrow do
  i <- anyPrim endian proxy
  if f i then pure i
  else fail "satisfy predicate failed"

-- | Parse one 8-bit signed integer that satisfies the given predicate.
satisfyInt8 :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt8 = satisfy LE (Proxy :: Proxy Int8)

-- | Parse one 16-bit big-endian signed integer that satisfies the given predicate.
satisfyInt16be :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt16be = satisfy BE (Proxy :: Proxy Int16)

-- | Parse one 16-bit little-endian signed integer that satisfies the given predicate.
satisfyInt16le :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt16le = satisfy LE (Proxy :: Proxy Int16)

-- | Parse one 32-bit big-endian signed integer that satisfies the given predicate.
satisfyInt32be :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt32be = satisfy BE (Proxy :: Proxy Int32)

-- | Parse one 32-bit little-endian signed integer that satisfies the given predicate.
satisfyInt32le :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt32le = satisfy LE (Proxy :: Proxy Int32)

-- | Parse one 8-bit unsigned integer that satisfies the given predicate.
satisfyUint8 :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint8 = satisfy LE (Proxy :: Proxy Uint8)

-- | Parse one 16-bit big-endian unsigned integer that satisfies the given predicate.
satisfyUint16be :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint16be = satisfy BE (Proxy :: Proxy Uint16)

-- | Parse one 16-bit little-endian unsigned integer that satisfies the given predicate.
satisfyUint16le :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint16le = satisfy LE (Proxy :: Proxy Uint16)

-- | Parse one 32-bit big-endian unsigned integer that satisfies the given predicate.
satisfyUint32be :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint32be = satisfy BE (Proxy :: Proxy Uint32)

-- | Parse one 32-bit little-endian unsigned integer that satisfies the given predicate.
satisfyUint32le :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint32le = satisfy LE (Proxy :: Proxy Uint32)

-- | Parse one 32-bit big-endian floating-point number that satisfies the given predicate.
satisfyFloat32be :: forall m. MonadEffect m => (Float32.Float32 -> Boolean) -> ParserT DataView m Float32.Float32
satisfyFloat32be = satisfy BE (Proxy :: Proxy Float32)

-- | Parse one 32-bit little-endian floating-point number that satisfies the given predicate.
satisfyFloat32le :: forall m. MonadEffect m => (Float32.Float32 -> Boolean) -> ParserT DataView m Float32.Float32
satisfyFloat32le = satisfy LE (Proxy :: Proxy Float32)

-- | Parse one 64-bit big-endian floating-point number that satisfies the given predicate.
satisfyFloat64be :: forall m. MonadEffect m => (Number -> Boolean) -> ParserT DataView m Number
satisfyFloat64be = satisfy BE (Proxy :: Proxy Float64)

-- | Parse one 64-bit little-endian floating-point number that satisfies the given predicate.
satisfyFloat64le :: forall m. MonadEffect m => (Number -> Boolean) -> ParserT DataView m Number
satisfyFloat64le = satisfy LE (Proxy :: Proxy Float64)

-- | Take *N* bytes starting from the current parser position. Will fail
-- | if not enough bytes remain in the input. Will fail if *N* is negative.
-- |
-- | #### Example
-- |
-- | Parse three bytes.
-- |
-- |     takeN 3
-- |
takeN :: forall m. MonadEffect m => ByteLength -> ParserT DataView m DataView
takeN n = do
  ParseState input (Position { index }) _ <- getParserT
  if (n < 0) then
    fail "takeN cannot take negative number of bytes"
  else if (index + n > DV.byteLength input) then
    fail "takeN expected N bytes"
  else do
    p <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + index) n
    stateParserT \_ -> Tuple p $ ParseState input (Position { line: 1, column: 1, index: index + n }) true

-- | Take the rest of the input, however many bytes remain. Always succeeds.
takeRest :: forall m. MonadEffect m => ParserT DataView m DataView
takeRest = do
  ParseState input (Position { index }) _ <- getParserT
  p <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + index)
    (DV.byteLength input - index)
  stateParserT \_ -> Tuple p $ ParseState input (Position { line: 1, column: 1, index: DV.byteLength input + 1 }) true

-- | Parse succeeds at the end of the input DataView.
eof :: forall m. Monad m => ParserT DataView m Unit
eof = do
  ParseState input (Position { index }) _ <- getParserT
  if (index + 1 > DV.byteLength input) then do
    -- We must consume so this combines correctly with notFollowedBy
    consume
  else do
    fail "eof expected end of DataView"

-- | The
-- | [famous `match`](http://www.serpentine.com/blog/2014/05/31/attoparsec/#from-strings-to-buffers-and-cursors)
-- | combinator.
-- |
-- | Return both the result of a parse and the portion of the input that
-- | was consumed while it was being parsed.
match :: forall a m. MonadEffect m => ParserT DataView m a -> ParserT DataView m (Tuple DataView a)
match p = do
  ParseState input (Position { index: index0 }) _ <- getParserT
  x <- p
  ParseState _ (Position { index: index1 }) _ <- getParserT
  part <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + index0) (index1 - index0)
  pure $ Tuple part x

-- | Combinator which finds the first position in the input `DataView` where the
-- | phrase can parse. Returns both the
-- | parsed result and the unparsable input section searched before the parse.
-- | Will fail if no section of the input is parseable. To backtrack the input
-- | stream on failure, combine with `tryRethrow`.
-- |
-- | This combinator is equivalent to `manyTill_ anyInt8`, but it will be
-- | faster because it returns a slice of the input `DataView` for the
-- | section preceding the parse instead of a `List Int`.
-- |
-- | Be careful not to look too far
-- | ahead; if the phrase parser looks to the end of the input then `anyTill`
-- | could be *O(n²)*.
anyTill
  :: forall m a
   . MonadEffect m
  => ParserT DataView m a
  -> ParserT DataView m (Tuple DataView a)
anyTill p = do
  ParseState input1 (Position { index: index0 }) _ <- getParserT
  Tuple index1 t <- tailRecM go unit
  part <- lift $ liftEffect $ DV.part (DV.buffer input1) (DV.byteOffset input1 + index0) (index1 - index0)
  pure $ Tuple part t
  where
  go unit = alt
    ( do
        ParseState _ (Position { index: index1 }) _ <- getParserT
        t <- try p
        pure $ Done $ Tuple index1 t
    )
    ( do
        _ <- anyInt8
        pure $ Loop unit
    )

-- ****************************** Notes ****************************************
--
-- We cannot have a primitive parser which parses a `DataView` and produces
-- an `ArrayView` (a Javascript Typed Array). Javascript DataViews are
-- endianess-aware, but Javascript Typed Arrays assume the native endianness
-- of the local machine. DataViews are intended to be used for I/O, ArrayViews
-- are intended to be used internally for graphics in a process, and they're
-- not intended to be both applied to the same ArrayBuffer.
-- The exception: `Uint8Array`
--
-- The failure messages are all constant strings form performance reasons.
-- If failure messages were constructed lazily then we could have more
-- descriptive messages.