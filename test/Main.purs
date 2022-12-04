module Test.Main where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Cast (fromUint8Array, toUint8Array)
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Typed (fromArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List (fromFoldable)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Exception (catchException, message)
import Parsing (ParseError(..), ParserT, Position(..), fail, liftExceptT, parseErrorPosition, runParserT)
import Parsing.Combinators (many, manyTill)
import Parsing.DataView (anyInt8, anyTill, eof, satisfyInt8)
import Parsing.DataView as DV
import Parsing.DataView.Basic (anyCodePointUTF8)
import Test.Assert (assert', assertEqual')
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.TextEncoder as TextEncoder
import Web.Encoding.UtfLabel as UtfLabel

parseTestT :: forall s a. Show a => Eq a => s -> a -> ParserT s Effect a -> Effect Unit
parseTestT input expected p = do
  result <- runParserT input p
  case result of
    Right actual -> do
      assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
      logShow actual
    Left err -> assert' (show err) false

parseFailT :: forall s a. s -> Position -> ParserT s Effect a -> Effect Unit
parseFailT input failPos p = do
  result <- runParserT input p
  case result of
    Right _ -> assert' ("ParseError expected at " <> show failPos) false
    Left err -> do
      let pos = parseErrorPosition err
      assert' ("ParseError expected at " <> show failPos <> " but parser failed at " <> show pos)
        (pos == failPos)
      logShow $ show failPos

mkPos :: Int -> Position
mkPos n = Position { index: n, line: 1, column: 1 }

main :: Effect Unit
main = do

  -- Test input is DataView = [5,6,7,8,9]
  ab <- AB.empty 5
  let dv = DataView.whole ab
  for_ [ 0, 1, 2, 3, 4 ] $ \i -> DataView.setInt8 dv i (i + 5)
  -- anyInt8
  parseTestT dv 5 DV.anyInt8
  -- manyTill eof
  parseTestT dv (fromFoldable [ 5, 6, 7, 8, 9 ]) $ manyTill DV.anyInt8 DV.eof
  -- manyTill satisfy
  parseTestT dv (fromFoldable [ 5, 6, 7, 8 ]) $ manyTill DV.anyInt8 (DV.satisfyInt8 (_ == 9))
  -- anyInt16be
  parseTestT dv 0x0506 DV.anyInt16be
  -- anyInt16le
  parseTestT dv 0x0605 DV.anyInt16le
  -- anyUint16be
  parseTestT dv (fromInt 0x0506) DV.anyUint16be
  -- anyUint16le
  parseTestT dv (fromInt 0x0605) DV.anyUint16le
  -- fail on anyInt16le past end of DataView
  parseFailT dv (mkPos 4) $ DV.anyInt16le *> DV.anyInt16le *> DV.anyInt16le
  -- anyInt32be and anyInt8
  parseTestT dv (Tuple 0x05060708 0x09) $ do
    l <- DV.anyInt32be
    r <- DV.anyInt8
    DV.eof
    pure $ Tuple l r
  -- anyInt32le and anyInt8
  parseTestT dv (Tuple 0x08070605 0x09) $ do
    l <- DV.anyInt32le
    r <- DV.anyInt8
    DV.eof
    pure $ Tuple l r
  -- anyUint32be and anyInt8
  parseTestT dv (Tuple (fromInt 0x05060708) 0x09) $ do
    l <- DV.anyUint32be
    r <- DV.anyInt8
    DV.eof
    pure $ Tuple l r
  -- anyUint32le and anyInt8
  parseTestT dv (Tuple (fromInt 0x08070605) 0x09) $ do
    l <- DV.anyUint32le
    r <- DV.anyInt8
    DV.eof
    pure $ Tuple l r
  -- takeN
  parseTestT dv 0x0908 do
    _ <- DV.takeN 3
    DV.anyInt16le <* DV.eof
  -- takeRest then a new parse then anyInt16le
  parseTestT dv 0x0908 do
    _ <- DV.takeN 3
    remain <- DV.takeRest
    lift (runParserT remain DV.anyInt16le) >>= case _ of
      Right actual -> pure actual
      Left err -> fail $ show err
  -- fail on takeN past end of DataView
  parseFailT dv (mkPos 0) $ DV.takeN 6
  -- takeRest then new parse on takeRest then anyInt8
  parseTestT dv 0x07 do
    _ <- DV.takeN 1
    DV.takeRest >>= \dv2 ->
      lift (runParserT dv2 $ DV.takeN 1 *> DV.takeRest) >>= case _ of
        Left err -> fail $ show err
        Right dv3 -> lift (runParserT dv3 $ DV.anyInt8) >>= case _ of
          Left err -> fail $ show err
          Right x -> pure x
  -- takeN then a new parse then fail on anyInt16le past end of DataView
  parseFailT dv (mkPos 1) $ do
    dv2 <- DV.takeN 1
    lift (runParserT dv2 DV.anyInt16le) >>= case _ of
      Left err -> fail $ show err
      Right x -> pure x
  parseTestT dv unit $ DV.takeN 5 *> pure unit

  -- test match combinator
  runParserT dv (DV.match (DV.anyInt8 *> DV.anyInt8)) >>= case _ of
    Right (Tuple matchresult _) -> do
      r0 <- DataView.getInt8 matchresult 0
      r1 <- DataView.getInt8 matchresult 1
      assert' ("match failed " <> show r0 <> " " <> show r1) $ r0 == Just 5 && r1 == Just 6
      logShow $ show r0 <> " " <> show r1
    Left err -> assert' ("match failed " <> show err) false

  do
    ablarge <- AB.empty 200000
    let dvlarge = DataView.whole ablarge
    -- forE 0 (AB.byteLength ablarge - 1) \i -> void $ DataView.setInt8 dvlarge i i
    x <- map List.length <$> runParserT dvlarge (many anyInt8)
    assertEqual' "stack safety"
      { actual: x
      , expected: Right 200000
      }

  do
    let
      teststring = "test string 👓"

    textDecoder <- TextDecoder.new UtfLabel.utf8

    textEncoder <- TextEncoder.new

    let testarray = TextEncoder.encode teststring textEncoder

    testview <- fromUint8Array testarray

    result <- runParserT testview do
      let length = DataView.byteLength testview
      stringview <- DV.takeN length
      stringarray <- lift $ liftEffect $ toUint8Array stringview
      liftExceptT $ ExceptT $ catchException (pure <<< Left <<< message) do
        Right <$> TextDecoder.decode stringarray textDecoder

    assertEqual' "UTF-8 decoding example"
      { expected: Right teststring
      , actual: result
      }

  do
    result <- runParserT dv do
      Tuple pre _ <- anyTill do
        _ <- satisfyInt8 (_ == 7)
        _ <- satisfyInt8 (_ == 8)
        pure unit
      pure pre

    assertEqual' "anyTill"
      { expected: Right 2
      , actual: DataView.byteLength <$> result
      }

  do
    -- Test one code point from each byte group
    -- a 97
    -- λ 955
    -- 月 0x6708
    -- 👓 0x1F453
    let teststring = "aλ月👓a"
    textEncoder <- TextEncoder.new
    let testarray = TextEncoder.encode teststring textEncoder
    testview <- fromUint8Array testarray

    result <- (map <<< map <<< map) fromEnum $ runParserT testview do
      c0 <- anyCodePointUTF8
      c1 <- anyCodePointUTF8
      c2 <- anyCodePointUTF8
      c3 <- anyCodePointUTF8
      c4 <- anyCodePointUTF8
      eof
      pure [ c0, c1, c2, c3, c4 ]

    assertEqual' "anyCodePointUTF8 basic"
      { expected: Right [ 97, 955, 0x6708, 0x1F453, 97 ]
      , actual: result
      }

  do
    test :: Uint8Array <- fromArray (map fromInt [ 0x00BF ])
    testv <- fromUint8Array test
    result <- runParserT testv anyCodePointUTF8
    assertEqual' "anyCodePointUTF8 invalid 1"
      { expected: Left (ParseError "Invalid UTF-8 encoding" (Position { index: 0, column: 1, line: 1 }))
      , actual: result
      }

  do
    test :: Uint8Array <- fromArray (map fromInt [ 0x00C0, 0 ])
    testv <- fromUint8Array test
    result <- runParserT testv anyCodePointUTF8
    assertEqual' "anyCodePointUTF8 invalid 2"
      { expected: Left (ParseError "Invalid UTF-8 encoding" (Position { index: 0, column: 1, line: 1 }))
      , actual: result
      }
