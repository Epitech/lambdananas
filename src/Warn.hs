{-|
Warnings that can be emitted by the rules checker functions.
-}
module Warn (
  Issue (..),
  Warn (..),
  Gravity (..),
  issues,
) where

-- | A coding style warning that can be emitted by a rule's check function.
data Warn = Warn { what :: Issue            -- ^ The issue raised
                 , loc :: (FilePath, Int)   -- ^ The location of the issue
                 , meta :: Maybe [String]   -- ^ Metadata to pass to the output 'String'
                 } deriving Eq

instance Ord Warn where
  compare (Warn _ (s1,l1) _) (Warn _ (s2,l2) _) | s1 == s2 = compare l1 l2
                                            | otherwise = compare s1 s2

-- | Describes an 'Issue' gravity.
data Gravity = Info | Minor | Major deriving Eq

instance Show Gravity where
  show Info = "INFO"
  show Minor = "MINOR"
  show Major = "MAJOR"

-- | Enumeration of all possible style issues arising from a program.
data Issue = BadIf                -- ^ Nested ifs
           | BadDo                -- ^ Useless do
           | BadReturn            -- ^ Useless generator
           | BadGuard             -- ^ Guard should be a pattern
           | LineTooLong          -- ^ Line too long
           | FunctionTooBig       -- ^ Function too big
           | NoSig                -- ^ No signature
           | NotParsable          -- ^ File is not parsable
           | ForbiddenExt         -- ^ File contains forbidden extension
           | Debug                -- ^ Debug
           deriving Eq

-- | Data linked to every issue
data IssueData = IssueData { gravity :: Gravity                 -- ^ Gravity of the issue
                           , code :: String                     -- ^ The code of the issue
                           , desc :: Maybe [String] -> String   -- ^ A function returning a short description and taking metadatas
                           , hint :: String }                   -- ^ A hint about how the student can solve the issue

type Code = String
type Hint = String
type Description = Maybe [String] -> String

issues :: [(Issue, IssueData)]
issues = [(BadIf, dataBadIf),
          (BadDo, dataBadDo),
          (BadReturn, dataBadReturn),
          (BadGuard, dataBadGuard),
          (LineTooLong, dataLineTooLong),
          (FunctionTooBig, dataFunctionTooBig),
          (NoSig, dataNoSig),
          (NotParsable, dataNotParsable),
          (ForbiddenExt, dataForbiddenExt),
          (Debug, dataDebug)]

dataBadIf :: IssueData
dataBadIf = IssueData
  Major
  "C1"
  (const "nested if")
  "ifs constructs should not be nested"

dataBadDo :: IssueData
dataBadDo = IssueData
  Major
  "D1"
  (const "useless do")
  "do structures should be avoided when no generator are used"

dataBadReturn :: IssueData
dataBadReturn = IssueData
  Minor
  "D2"
  (const "useless generator")
  "do structures should not have useless return statements"

dataBadGuard :: IssueData
dataBadGuard = IssueData
  Major
  "C2"
  (const "guard should be a pattern")
  "guard constructs should only be used if it cannot be a pattern match"

dataLineTooLong :: IssueData
dataLineTooLong = IssueData
  Minor
  "F3"
  (const "line too long")
  "lines should be less than 80 columns wide"

dataFunctionTooBig :: IssueData
dataFunctionTooBig = IssueData
  Minor
  "F4"
  (const "function too big")
  "every functions definitions should be less than 10 lines"

dataNoSig :: IssueData
dataNoSig = IssueData
  Minor
  "T1"
  description
  "top-level declaration has no corresponging type signature"
  where
    description (Just (s:_)) = s ++ " has no signature"
    description _ = "function has no signature"

dataForbiddenExt :: IssueData
dataForbiddenExt = IssueData
  Major
  "E1"
  description
  "line too long"
  where
    description (Just (s:_)) = s ++ " contains forbidden extensions"
    description _ = "a file contains forbidden extensions"

dataNotParsable :: IssueData
dataNotParsable = IssueData
  Major
  "P1"
  description
  "all files must be parsable without extensions"
  where
    description (Just (s:_)) = s ++ " could not be parsed"
    description _ = "a file could not be parsed"

dataDebug :: IssueData
dataDebug = IssueData
  Info
  "XX"
  description
  "debug rule found something, this should not happen!"
  where
    description (Just (s:_)) = s
    description _ = "debug rule left empty"
