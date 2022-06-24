{-|
GHC parsing wrapper.
-}
module ParserWrapper (
  ParseError (..),
  parseFile,
) where

import Lexer
import Parser
import SrcLoc
import FastString
import StringBuffer

import ParserSettings
import Common hiding (loc)

-- | Parse a file.
-- Basically a wrapper around 'parseHS'.
parseFile :: FilePath
          -> IO (Either ParseError ParseSuccess)
parseFile file = do
    content <- readFile file
    case runParser file content parseModule of
      POk s (L _ m) ->
        return $ Right $ ParseSuccess m $ comment_q s
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

