-- Clojure's instaparse is great for context-free grammars, but this grammar is not context free.
-- Specifically, note that Parsec can easily evaluate packets to determine where to parse next.
-- In instaparse this would require interleaving evaluation and parsing in an unnatural way.

{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude hiding (try, (<|>), head)
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec (Parser(..), parse, try, (<|>), count, digit, string, many1)
import Data.Text (unpack)
import Data.List.NonEmpty (NonEmpty(..), head, last)

data Packet = Lit Int Int | Op Int Int (NonEmpty Packet) | Error deriving (Show)

main :: IO ()
main = do
  inputFile <- readFile "input" :: IO Text
  let binput = concat $ hexToBin <$> unpack inputFile
  case parse parsePacket "Parser" binput of
    Right packet -> do
      print $ versionSum packet
      print $ evaluatePacket packet
    Left err -> print err

hexToBin :: Char -> [Char]
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

binToDec :: [Char] -> Int
binToDec = foldl (\acc dig -> digitToInt dig + 2 * acc) 0

digits :: Int -> Parser Int
digits n = binToDec <$> count n digit

parsePacket :: Parser Packet
parsePacket = try parseLiteral <|> parseOp

parseLiteral :: Parser Packet
parseLiteral = do
  version <- binToDec <$> count 3 digit
  typeId <- binToDec <$> string "100"
  num <- parseLiteralNum
  pure . Lit version $ binToDec num

parseLiteralNum :: Parser [Char]
parseLiteralNum = do
  header <- digit
  case header of
    '1' -> (++) <$> count 4 digit <*> parseLiteralNum
    '0' -> count 4 digit

parseOp :: Parser Packet
parseOp = do
  version <- digits 3
  typeId <- digits 3
  lengthTypeId <- digits 1
  case lengthTypeId of
    0 -> do
      l <- digits 15
      packetSection <- count l digit
      case parse (many1 parsePacket) "Parser" packetSection of
        Right (p:ps) -> pure $ Op version typeId (p :| ps)
        Right [] -> pure Error
        Left err -> pure Error
    1 -> do
      numPackets <- digits 11
      packets <- count numPackets parsePacket
      pure $ maybe Error (Op version typeId) (nonEmpty packets)

versionSum :: Packet -> Int
versionSum (Lit vsn num) = vsn
versionSum (Op vsn typeId packets) = (+) vsn $ sum $ versionSum <$> packets

evaluatePacket :: Packet -> Int
evaluatePacket (Lit vsn num) = num
evaluatePacket (Op vsn typeId packets)
  | typeId == 0 = sum $ evaluatePacket <$> packets
  | typeId == 1 = product $ evaluatePacket <$> packets
  | typeId == 2 = minimum $ evaluatePacket <$> packets
  | typeId == 3 = maximum $ evaluatePacket <$> packets
  | typeId == 5 && evaluatePacket (head packets) > evaluatePacket (last packets) = 1
  | typeId == 6 && evaluatePacket (head packets) < evaluatePacket (last packets) = 1
  | typeId == 7 && evaluatePacket (head packets) == evaluatePacket (last packets) = 1
  | otherwise = 0
