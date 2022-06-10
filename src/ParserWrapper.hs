{-|
Parsing wrapper and inspection functions.
-}
module ParserWrapper (
  ParseError (..),
  parseFile,
) where

import Common (Literal(String))

import Lexer
import Parser
import ParserSettings
import SrcLoc
import FastString
import StringBuffer

data ParseError = ParseError { filename :: String
                             , line :: Int
                             , column :: Int
                             } deriving Eq

instance Show ParseError where
  show (ParseError f l c) = "Failed to parse at : " ++ f ++ ' ':show l ++ ':':show c

-- | Parse a file.
-- Basically a wrapper around 'parseHS'.
parseFile :: FilePath                       -- ^ File to be parsed
          -> IO (Either ParseError String)  -- ^ An error or a list of top level declarations
parseFile file = do
    content <- readFile file
    case runParser file content parseModule of
      POk _ _ ->
        return $ Right "parsed"
      PFailed PState {loc = l} ->
        return $ Left $ ParseError file (srcLocLine l) (srcLocCol l)


-- | Parse a string.
runParser :: FilePath -- ^ Name of the parsed file
          -> String   -- ^ String to parse
          -> P a      -- ^ Kind of parser to use
          -> ParseResult a
runParser file str parser = unP parser parserState
  where
    location = mkRealSrcLoc (mkFastString file) 1 1
    content = stringToStringBuffer str
    parserState = mkPStatePure parserFlags content location


