{-|
Warnings that can be emitted by the rules checker functions.
-}
module Warn (
  Issue (..),
  IssueInfo (..),
  Warn (issue, loc, arg),
  makeWarn,
  unsafeMakeWarn,
  Gravity (..),
  issues,
) where
import Common (Literal(String))

-- | A coding style warning that can be emitted by a rule's check function.
data Warn = Warn { issue :: Issue           -- ^ The issue raised
                 , loc :: (FilePath, Int)   -- ^ The location of the issue
                 , arg :: IssueArg          -- ^ Arguments to pass to the output functions
                 } deriving Eq

instance Ord Warn where
  compare (Warn _ (s1,l1) _) (Warn _ (s2,l2) _) | s1 == s2 = compare l1 l2
                                            | otherwise = compare s1 s2
-- | 'Warn' smart constructor.
-- Checks that there is no wrong Issue / Arg combination.
-- See https://wiki.haskell.org/Smart_constructors
makeWarn :: Issue -> (FilePath, Int) -> IssueArg -> Warn
makeWarn i@NoSig l a@(StringArg _) = Warn i l a
makeWarn _ _ _= error "invalid Issue/Arg combination"

-- | 'Warn' dumb constructor. Disables all checks.
-- Please use 'makeWarn' instead of this.
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
           | Debug                -- ^ Debug
           deriving Eq


-- | Data linked to every issue.
data IssueInfo = IssueInfo { gravity :: Gravity
                           -- ^ Gravity of the issue
                           , code :: String
                           -- ^ The code of the issue
                           , showDetails :: IssueArg -> String
                           -- ^ A function returning a short description and taking metadatas
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
          (Debug, dataDebug)]

dataBadIf :: IssueInfo
dataBadIf = IssueInfo
  Major
  "C1"
  (const "nested if")
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
  Minor
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
    description (StringArg s) = s ++ " could not be parsed"
    description _ = "a file could not be parsed"

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
