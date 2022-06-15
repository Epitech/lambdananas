{-|
Warnings that can be emitted by the rules checker functions.
-}
module Warn (
  Warn (..),
  makeWarn,
  unsafeMakeWarn,
  lookupIssueInfo,
  issues,
  Issue (..),
  IssueInfo (..),
  IssueArg (..),
  Gravity (..),
) where

import Data.Maybe

-- | A coding style warning that can be emitted by a rule's check function.
data Warn = Warn { issue :: Issue           -- ^ The issue raised
                 , loc :: (FilePath, Int)   -- ^ The location of the issue
                 , arg :: IssueArg          -- ^ Arguments to pass to the output functions
                 } deriving (Eq, Show)

instance Ord Warn where
  compare (Warn _ (s1,l1) _) (Warn _ (s2,l2) _) | s1 == s2 = compare l1 l2
                                            | otherwise = compare s1 s2
-- | 'Warn' smart constructor.
-- Checks that there is no wrong Issue / Arg combination.
-- See https://wiki.haskell.org/Smart_constructors
makeWarn :: Issue -> (FilePath, Int) -> IssueArg -> Warn
makeWarn i@BadIf l NoArg = Warn i l NoArg
makeWarn i@BadDo l NoArg = Warn i l NoArg
makeWarn i@BadReturn l NoArg = Warn i l NoArg
makeWarn i@BadGuard l NoArg = Warn i l NoArg
makeWarn i@LineTooLong l NoArg = Warn i l NoArg
makeWarn i@FunctionTooBig l NoArg = Warn i l NoArg
makeWarn i@NoSig l a@(StringArg _) = Warn i l a
makeWarn i@NotParsable l a@(StringArg _) = Warn i l a
makeWarn i@ForbiddenExt l a@(StringArg _) = Warn i l a
makeWarn i@BadHeader l a@(StringArg _) = Warn i l a
makeWarn i@Debug l a@(StringArg _) = Warn i l a
makeWarn _ _ _= error "invalid Issue/Arg combination"


-- Keeping this because it makes it clear that using the
-- data constructor is not the right way.
-- | 'Warn' dumb constructor. Disables all checks.
-- Only usage should be for dumping manifest.
unsafeMakeWarn :: Issue -> (FilePath, Int) -> IssueArg -> Warn
unsafeMakeWarn = Warn

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
           | BadHeader             -- ^ File has no header
           | Debug                -- ^ Debug
           deriving (Eq, Show)


-- | Data linked to every issue.
data IssueInfo = IssueInfo { gravity :: Gravity
                           -- ^ Gravity of the issue
                           , code :: String
                           -- ^ The code of the issue
                           , showDetails :: IssueArg -> String
                           -- ^ A function returning a short description and taking
                           -- metadatas
                           , hint :: String
                           -- ^ A hint about how the student can solve the issue
                           }

-- | Arguments to be given to an issue.
data IssueArg = NoArg | StringArg String deriving (Show, Eq)

-- | Describes a lookup table linking 'Issue' to
-- their corresponding 'IssueInfo'.
issues :: [(Issue, IssueInfo)]
issues = [(BadIf, dataBadIf),
          (BadDo, dataBadDo),
          (BadReturn, dataBadReturn),
          (BadGuard, dataBadGuard),
          (LineTooLong, dataLineTooLong),
          (FunctionTooBig, dataFunctionTooBig),
          (NoSig, dataNoSig),
          (NotParsable, dataNotParsable),
          (ForbiddenExt, dataForbiddenExt),
          (BadHeader, dataBadHeader),
          (Debug, dataDebug)]

lookupIssueInfo :: Issue -> IssueInfo
lookupIssueInfo i = fromMaybe dataDebug $ lookup i issues

dataBadIf :: IssueInfo
dataBadIf = IssueInfo
  Major
  "C1"
  (const "nested ifs")
  "ifs constructs should not be nested"

dataBadDo :: IssueInfo
dataBadDo = IssueInfo
  Major
  "D1"
  (const "useless do")
  "do structures should be avoided when no generator are used"

dataBadReturn :: IssueInfo
dataBadReturn = IssueInfo
  Minor
  "D2"
  (const "useless generator")
  "do structures should not have useless return statements"

dataBadGuard :: IssueInfo
dataBadGuard = IssueInfo
  Major
  "C2"
  (const "guard should be a pattern")
  "guard constructs should only be used if it cannot be a pattern match"

dataLineTooLong :: IssueInfo
dataLineTooLong = IssueInfo
  Minor
  "F3"
  (const "line too long")
  "lines should be less than 80 columns wide"

dataFunctionTooBig :: IssueInfo
dataFunctionTooBig = IssueInfo
  Minor
  "F4"
  (const "function too big")
  "every functions definitions should be less than 10 lines"

dataNoSig :: IssueInfo
dataNoSig = IssueInfo
  Major
  "T1"
  description
  "top-level declaration has no corresponding type signature"
  where
    description (StringArg s) = s ++ " has no signature"
    description _ = "function has no signature"

dataForbiddenExt :: IssueInfo
dataForbiddenExt = IssueInfo
  Major
  "E1"
  description
  "line too long"
  where
    description (StringArg s) = s ++ " contains forbidden extensions"
    description _ = "a file contains forbidden extensions"

dataNotParsable :: IssueInfo
dataNotParsable = IssueInfo
  Major
  "P1"
  description
  "all files must be parsable without extensions"
  where
    description (StringArg s) = s ++ " is not parsable"
    description _ = "a file is not parsable"

dataBadHeader :: IssueInfo
dataBadHeader = IssueInfo
  Major
  "G1"
  description
  "file must start with a correctly formatted Epitech standard header"
  where
    description (StringArg s) = s ++ " has a badly formatted Epitech header"
    description _ = "a file has a badly formatted Epitech header"

dataDebug :: IssueInfo
dataDebug = IssueInfo
  Info
  "XX"
  description
  "debug rule found something, this should not happen!"
  where
    description (StringArg s) = s
    description _ = "debug rule left empty"

-- | Describes an 'Issue' gravity.
data Gravity = Info | Minor | Major deriving Eq

instance Show Gravity where
  show Info = "INFO"
  show Minor = "MINOR"
  show Major = "MAJOR"
