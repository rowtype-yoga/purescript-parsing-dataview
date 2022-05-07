-- | Primitive parsers for parsing Javascript ArrayBuffers with the
-- | [`Text.Parsing.Parser`](https://pursuit.purescript.org/packages/purescript-parsing/docs/Text.Parsing.Parser)
-- | module in package __purescript-parsing__.
-- | See the package README for usage examples.
-- |
-- | All of the parsers in this module operate on an input stream of
-- | [`Data.ArrayBuffer.Types.DataView`](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:DataView),
-- | which represents a range of an
-- | `Data.ArrayBuffer.Types.ArrayBuffer`.
-- |
-- | For operations for working with `ArrayBuffer` and `DataView`, see
-- | module
-- | [`Data.ArrayBuffer.DataView`](https://pursuit.purescript.org/packages/purescript-arraybuffer/docs/Data.ArrayBuffer.DataView)
-- | in package __purescript-arraybuffer__.
-- |
-- | Reading from an `ArrayBuffer` is an `Effect`ful activity, so
-- | all parsers in this module must be run in a
-- | `MonadEffect m => ParserT DataView m` context, with
-- | `Text.Parsing.Parser.runParserT`.
module Text.Parsing.Parser.DataView
  ( anyPrim
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
  , takeN
  , takeRest
  , eof
  , match
  )
  where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (get, put)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Tuple(Tuple(..))
import Data.UInt (UInt)
import Data.Float32 as Float32
import Effect.Class (class MonadEffect, liftEffect)
import Type.Proxy (Proxy(..))

import Text.Parsing.Parser (ParserT, ParseState(..), fail)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.Combinators (tryRethrow)
import Data.ArrayBuffer.Types
  ( DataView
  , Int16
  , Int32
  , Int8
  , Uint16
  , Uint32
  , Uint8
  , Float32
  , Float64
  , ByteLength
  )
import Data.ArrayBuffer.ValueMapping
  ( class BytesPerType
  , class BinaryValue
  , class ShowArrayViewType
  , byteWidth
  )
import Data.ArrayBuffer.DataView
  ( Endian(LE,BE)
  )
import Data.ArrayBuffer.DataView (byteLength, byteOffset, get, part, buffer) as DV

-- | Parse one fixed-bit-width `Data.ArrayBuffer.Types.ArrayViewType` primitive
-- | of a given endianness.
-- |
-- | #### Example
-- |
-- | Parse a little-endian 32-bit signed integer (4 bytes):
-- |
-- |     anyPrim LE (Proxy :: Proxy Int32)
-- |
-- | or just use the convenience function `anyInt32le`, see below.
anyPrim :: forall a name m t
       . BinaryValue a t
      => BytesPerType a
      => ShowArrayViewType a name
      => IsSymbol name
      => MonadEffect m
      => Endian
      -> Proxy a
      -> ParserT DataView m t
anyPrim endian _ = do
  ParseState input (Position{line,column}) _ <- get
  lift (liftEffect (DV.get endian (Proxy :: Proxy a) input (column-1))) >>= case _ of
    Nothing -> fail $ "Cannot parse " <> reflectSymbol (SProxy :: SProxy name) <>
      ", unexpected end of DataView"
    Just i -> do
      put $ ParseState input (Position {line, column:column + byteWidth (Proxy :: Proxy a) }) true
      pure i

-- | Parse one 8-bit signed integer (byte).
anyInt8 :: forall m. MonadEffect m => ParserT DataView m Int
anyInt8 = anyPrim LE (Proxy :: Proxy Int8)

-- | Parse one 16-bit big-endian signed integer.
anyInt16be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16be = anyPrim BE (Proxy :: Proxy Int16)

-- | Parse one 16-bit little-endian signed integer.
anyInt16le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16le = anyPrim LE (Proxy :: Proxy Int16)

-- | Parse one 32-bit big-endian signed integer.
anyInt32be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32be = anyPrim BE (Proxy :: Proxy Int32)

-- | Parse one 32-bit little-endian signed integer.
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

-- | Parse one 32-bit big-endian floating point number.
anyFloat32be :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32be = anyPrim BE (Proxy :: Proxy Float32)

-- | Parse one 32-bit little-endian floating point number.
anyFloat32le :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32le = anyPrim LE (Proxy :: Proxy Float32)

-- | Parse one 64-bit big-endian floating point number.
anyFloat64be :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64be = anyPrim BE (Proxy :: Proxy Float64)

-- | Parse one 64-bit little-endian floating point number.
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
satisfy :: forall a name m t
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
         else fail $ reflectSymbol (SProxy :: SProxy name) <>
                     " " <> show i <> " did not satisfy predicate."

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
  ParseState input (Position {line,column}) _ <- get
  unless (n >= 0) $ fail $ "Cannot take negative number of bytes."
  unless (column + n - 1 <= DV.byteLength input) $
    fail $ "Cannot take " <> show n <> " bytes, only " <>
      show (DV.byteLength input - column + 1) <> " bytes remain."
  p <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + (column-1)) n
  put $ ParseState input (Position {line,column:column+n}) true
  pure p

-- | Take the rest of the input, however many bytes remain. Always succeeds.
takeRest :: forall m. MonadEffect m => ParserT DataView m DataView
takeRest = do
  ParseState input (Position {line,column}) _ <- get
  p <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + (column-1))
                                                     (DV.byteLength input - (column-1))
  put $ ParseState input (Position {line,column:DV.byteLength input + 1}) true
  pure p

-- | Parse succeeds at the end of the input DataView. Consumes no input.
eof :: forall m. Monad m => ParserT DataView m Unit
eof = do
  ParseState input (Position {column}) _ <- get
  unless (column > DV.byteLength input) $ fail "Expected end of DataView"

-- | The
-- | [famous `match`](http://www.serpentine.com/blog/2014/05/31/attoparsec/#from-strings-to-buffers-and-cursors)
-- | combinator.
-- |
-- | Return both the result of a parse and the portion of the input that
-- | was consumed while it was being parsed.
match :: forall a m. MonadEffect m => ParserT DataView m a -> ParserT DataView m (Tuple DataView a)
match p = do
  ParseState input (Position {column:column0}) _ <- get
  x <- p
  ParseState _ (Position {column:column1}) _ <- get
  part <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + (column0-1)) (column1-column0)
  pure $ Tuple part x

-- ****************************** Notes ****************************************
--
-- The `initialPostion` in a `ParserT` is `Postion{line:1,column:1)`.
-- We keep `line` invariant and use `column-1` to denote the offset into the
-- `DataView`, that's why there are so many plus 1 and minus 1 operations
-- in this module.
-- https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html
--
-- We cannot have a primitive parser which parses a `DataView` and produces
-- an `ArrayView` (a Javascript Typed Array). Javascript DataViews are
-- endianess-aware, but Javascript Typed Arrays assume the native endianness
-- of the local machine. DataViews are intended to be used for I/O, ArrayViews
-- are intended to be used internally for graphics in a process, and they're
-- not intended to be both applied to the same ArrayBuffer.
