{-# LANGUAGE CPP, OverloadedStrings, BangPatterns #-}

#define COLON 58
#define SPACE 32
#define TAB   9
#define A_UPPER 65
#define A_LOWER 97
#define Z_UPPER 90
#define Z_LOWER 122
#define ZERO 48
#define NINE 57
#define PERIOD 46
#define UNDERSCORE 95

module Data.Array.Repa.Distributed.Config where

import Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word (Word8)

import Control.Applicative

data Id = Id !B.ByteString !B.ByteString !Int deriving (Eq, Show, Ord)

data Node = Node !Id
          deriving (Eq, Show)

data NodeRegistry = NodeRegistry { registry :: M.Map Id Node } deriving (Show)

parseConfig :: String -> IO (Either String NodeRegistry)
parseConfig conf = do
    bytes <- B.readFile conf
    return $ A.parseOnly config bytes
        
config :: Parser NodeRegistry
config = do
    n @ (Node id) <- configLine
    return $ NodeRegistry $ M.fromList [(id, n)]

eatHSpace :: Parser ()
eatHSpace = skipMany wspace
  where wspace = satisfy $ \c -> 
          c == SPACE || c == TAB

configLine :: Parser Node
configLine = do
    name <- nodeName <* eatHSpace
    string "=>" <* eatHSpace
    ip' <- (hostname <|> ip) <* colon
    p <- port
    return $ Node (Id name ip' p)

{-# INLINE colon #-}
colon = A.word8 COLON

letter_ascii :: Parser Word8
letter_ascii = satisfy $ \w ->
    (w >= A_LOWER && w <= Z_LOWER) || (w >= A_UPPER && w <= Z_UPPER)

digit :: Parser Word8
digit = satisfy $ \w -> w >= ZERO && w <= NINE
    
nodeName :: Parser B.ByteString
nodeName = B.pack <$> many1 (letter_ascii <|> digit <|> A.word8 UNDERSCORE)

hostname :: Parser B.ByteString 
hostname = B.pack <$> many (letter_ascii <|> digit <|> A.word8 PERIOD)

ip :: Parser B.ByteString
ip = takeWhile1 ipChar
  where ipChar w = (w >= ZERO && w <= NINE) || w == PERIOD

port :: Parser Int
port = return 10
