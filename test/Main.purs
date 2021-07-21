module Test.Main where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.ArrayBuffer.DataView as DataView
import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Console (logShow)
import Test.Assert (assert')
import Text.Parsing.Parser (ParserT, runParserT, parseErrorPosition, fail)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.DataView as DV
import Text.Parsing.Parser.Pos (Position(..))

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
    Right _ -> assert' ("ParseError expected at "  <> show failPos) false
    Left err -> do
      let pos = parseErrorPosition err
      assert' ("ParseError expected at " <> show failPos <> " but parser failed at " <> show pos)
              (pos == failPos)
      logShow $ show failPos

mkPos :: Int -> Position
mkPos n = mkPos' n 1

mkPos' :: Int -> Int -> Position
mkPos' column line = Position { column: column, line: line }


main :: Effect Unit
main = do

  -- Test input is DataView = [5,6,7,8,9]
  ab <- AB.empty 5
  let dv = DataView.whole ab
  for_ [0,1,2,3,4] $ \i -> DataView.setInt8 dv i (i+5)
  -- anyInt8
  parseTestT dv 5 DV.anyInt8
  -- manyTill eof
  parseTestT dv (fromFoldable [5,6,7,8,9]) $ manyTill DV.anyInt8 DV.eof
  -- manyTill satisfy
  parseTestT dv (fromFoldable [5,6,7,8]) $ manyTill DV.anyInt8 (DV.satisfyInt8 (_ == 9))
  -- anyInt16be
  parseTestT dv 0x0506 DV.anyInt16be
  -- anyInt16le
  parseTestT dv 0x0605 DV.anyInt16le
  -- anyUint16be
  parseTestT dv (fromInt 0x0506) DV.anyUint16be
  -- anyUint16le
  parseTestT dv (fromInt 0x0605) DV.anyUint16le
  -- fail on anyInt16le past end of DataView
  parseFailT dv (mkPos 5) $ DV.anyInt16le *> DV.anyInt16le *> DV.anyInt16le
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
  parseFailT dv (mkPos 1) $ DV.takeN 6
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
  parseFailT dv (mkPos 2) $ do
    dv2 <- DV.takeN 1
    lift (runParserT dv2 DV.anyInt16le) >>= case _ of
      Left err -> fail $ show err
      Right x -> pure x

  -- test match combinator
  runParserT dv (DV.match (DV.anyInt8 *> DV.anyInt8)) >>= case _ of
    Right (Tuple matchresult _) -> do
      r0 <- DataView.getInt8 matchresult 0
      r1 <- DataView.getInt8 matchresult 1
      assert' ("match failed " <> show r0 <> " " <> show r1) $ r0 == Just 5 && r1 == Just 6
      logShow $ show r0 <> " " <> show r1
    Left err -> assert' ("match failed " <> show err) false
