{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (null, head, tail, exponent, takeWhile, pred)

import Data.ByteString hiding (takeWhile)
import Data.Attoparsec.ByteString

type Keyword = ByteString
type Identifier = [ByteString]

data Sign = Positive | Negative
  deriving Show

data Object where
  Sexp :: Identifier -> [(Keyword, Object)] -> Object
  List :: [Object] -> Object
  String :: ByteString -> Object
  Symbol :: ByteString -> Object
  Boolean :: Bool -> Object
  Integer :: Sign -> Int -> Object
  Scientific :: Sign -> Int -> Int -> Object
  deriving Show

testInput :: ByteString
testInput = "(some:object :key \"hello\" :t #t :f #f :another-key (subtask :key \"omg\" :n -97 :sym hello :ll [1 2 3] :sci -23e77))"

testOutput :: Object
testOutput = Sexp ["some", "object"] [ ("key", String "hello")
                                     , ("t", Boolean True)
                                     , ("f", Boolean False)
                                     , ("another-key", another)]
  where another = Sexp ["subtask"] [ ("key", String "omg")
                                   , ("n", Integer Positive 97)
                                   , ("sym", Symbol "hello")
                                   , ("ll", List [Integer Positive 1, Integer Positive 2, Integer Positive 3])
                                   , ("sci", Scientific Positive 23 77)
                                   ]

--------------------------------------------------------------------------------
-- lexical items and filters

num :: String
num = "0-9"

alpha :: String
alpha = "a-zA-Z"

alphanum :: String
alphanum = "a-zA-Z0-9"

spacer :: String
spacer = " \n"

pos :: ByteString
pos = "+"

neg :: ByteString
neg = "-"

exponent :: ByteString
exponent = "e"

true :: ByteString
true = "#t"

false :: ByteString
false = "#f"

key :: ByteString
key = ":"

identSep :: ByteString
identSep = ":"

openSexp :: ByteString
openSexp = "("

closeSexp :: ByteString
closeSexp = ")"

openList :: ByteString
openList = "["

sepList :: ByteString
sepList = ","

closeList :: ByteString
closeList = "]"

openText :: ByteString
openText = "\""

closeText :: ByteString
closeText = "\""

--------------------------------------------------------------------------------
-- primitive ix types

whitespace :: Parser ()
whitespace = takeWhile1 (inClass spacer) *> return ()

ident :: Parser ByteString
ident = do h <- satisfy (inClass alpha)
           t <- takeWhile (inClass alphanum)
           return $ h `cons` t

symbol :: Parser Object
symbol = ident >>= (return . Symbol)

text :: Parser Object
text = do _ <- string openText
          t <- scan 0 pred
          _ <- string closeText
          return $ String t
  where pred s i = case i of
          34 -> case s of { 92 -> Just 34; _ -> Nothing }
          _ -> Just i

boolean :: Parser Object
boolean = do b <- choice [t, f]
             return b
  where t = do _ <- string true
               return $ Boolean True
        f = do _ <- string false
               return $ Boolean False

-- NOTE this would be faster and more clean to do on the stream instead of copying the bytestring
int :: ByteString -> Int
int b = case null b of
  True -> undefined
  False -> loop b 0
  where loop s a = case null s of
          True -> a
          False -> loop (tail s) (a * 10 + (fromEnum (head s) - 0x30))

sign :: Parser Sign
sign = choice [ string pos >> return Positive
              , string neg >> return Negative
              ]

integer :: Parser Object
integer = do s <- option Positive sign
             i <- takeWhile1 (inClass num)
             return $ Integer s (int i)

scientific :: Parser Object
scientific = do s <- option Positive sign
                i <- takeWhile1 (inClass num)
                _ <- string exponent
                e <- takeWhile1 (inClass num)
                return $ Scientific s (int i) (int e)

--------------------------------------------------------------------------------
-- basic grammar

identifier :: Parser Identifier
identifier = do hs <- many' prefix
                t <- ident
                return $ hs ++ [t]
  where prefix = do s <- ident
                    _ <- string ":"
                    return s

keyword :: Parser Keyword
keyword = do _ <- string ":"
             k <- ident
             return k

--------------------------------------------------------------------------------
-- composite grammar

pair :: Parser (Keyword, Object)
pair = do k <- keyword
          whitespace
          o <- object
          return (k, o)

sexp :: Parser Object
sexp = do _ <- string openSexp
          i <- identifier
          whitespace
          p <- manyTill pair (string closeSexp)
          return $ Sexp i p

list :: Parser Object
list = do _ <- string openList
          l <- object `sepBy` (string sepList >> whitespace)
          _ <- string closeList
          return $ List l

object :: Parser Object
object = choice [ sexp
                , list
                , text
                , symbol
                , boolean
                , scientific
                , integer
                ]

--------------------------------------------------------------------------------
-- entry

ix :: Parser Object
ix = sexp

--------------------------------------------------------------------------------
-- run test

in1 :: ByteString
in1 = "(s:x :a 1 :b 1e3) "

main :: IO ()
main = undefined
