-- | Basic `DataView` parsers derived from primitive `DataView` parsers.
-- |
-- | All of these parsers will consume when they succeed.
-- |
-- | All of these parsers will not consume and will automatically
-- | backtrack when they fail.
module Parsing.DataView.Basic
  ( anyCodePointUTF8
  ) where

import Prelude

import Data.ArrayBuffer.Types (DataView)
import Data.Enum (toEnum)
import Data.String (CodePoint)
import Data.UInt (toInt)
import Effect.Class (class MonadEffect)
import Parsing (ParserT, fail, liftMaybe)
import Parsing.Combinators (tryRethrow)
import Parsing.DataView (anyUint8)

-- | Decode one UTF-8 code point, *1—4* bytes.
anyCodePointUTF8
  :: forall m
   . MonadEffect m
  => ParserT DataView m CodePoint
anyCodePointUTF8 = tryRethrow do
  -- https://en.wikipedia.org/wiki/UTF-8#Encoding
  c0 <- toInt <$> anyUint8
  if c0 < 128 then do
    liftMaybe (\_ -> "Invalid CodePoint " <> show c0) (toEnum c0)
  else if c0 < 192 then fail "Invalid UTF-8 encoding"
  else if c0 < 224 then do
    c1 <- toInt <$> anyUint8
    if c1 < 128 || c1 > 191 then fail "Invalid UTF-8 encoding" else pure unit
    let codepoint = (64 * (c0 `mod` 32)) + (c1 `mod` 64)
    liftMaybe (\_ -> "Invalid CodePoint " <> show codepoint) (toEnum codepoint)
  else if c0 < 240 then do
    c1 <- toInt <$> anyUint8
    c2 <- toInt <$> anyUint8
    if c1 < 128 || c1 > 191 || c2 < 128 || c2 > 191 then fail "Invalid UTF-8 encoding" else pure unit
    let codepoint = (4096 * (c0 `mod` 16)) + (64 * (c1 `mod` 64)) + (c2 `mod` 64)
    liftMaybe (\_ -> "Invalid CodePoint " <> show codepoint) (toEnum codepoint)
  else if c0 < 248 then do
    c1 <- toInt <$> anyUint8
    c2 <- toInt <$> anyUint8
    c3 <- toInt <$> anyUint8
    if c1 < 128 || c1 > 191 || c2 < 128 || c2 > 191 || c3 < 128 || c3 > 191 then fail "Invalid UTF-8 encoding" else pure unit
    let codepoint = (262144 * (c0 `mod` 8)) + (4096 * (c1 `mod` 64)) + (64 * (c2 `mod` 64)) + (c3 `mod` 64)
    liftMaybe (\_ -> "Invalid CodePoint " <> show codepoint) (toEnum codepoint)
  else fail "Invalid UTF-8 encoding"
