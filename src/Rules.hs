{-|
Coding style rules.
-}
module Rules (
    Check,
    Warn (..),
    getIssueDesc,
    getIssuesList,
    Rule (..),
    Gravity(..),
    Issue (..),
    allRules,
    showLong,
    defaultRules,
    showArgo,
    showVera,
    checkSigs,
    checkIfs,
    checkReturns,
    checkDos,
    checkGuards,
    checkLines
) where

import Parser

import Language.Haskell.Exts.Syntax hiding (Rule)
import Language.Haskell.Exts.SrcLoc
import Control.Monad
import Control.Monad.Writer
import Data.Foldable

import BadIf
import NoSig
import BadDoReturn
import BadDo
import BadGuard
import LineTooLong
import Common

checkIfs = BadIf.check
checkSigs = NoSig.check
checkReturns = BadDoReturn.check
checkDos = BadDo.check
checkGuards = BadGuard.check
checkLines = LineTooLong.check

-- | Describes a coding style rule.
data Rule = Rule { name :: String         -- ^ Rule name
                 , _description :: String -- ^ Rule description
                 , getRule :: Check       -- ^ Function to check the rule
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

---- RULES ----

allRules :: [Rule]
allRules = [ ruleCheckSign, ruleCheckIfs, ruleCheckReturns,
            ruleCheckDos, ruleCheckGuards, ruleCheckLines ]

defaultRules :: [Rule]
defaultRules = [ ruleCheckSign, ruleCheckIfs, ruleCheckReturns,
                 ruleCheckDos, ruleCheckGuards, ruleCheckLines ]

ruleCheckSign :: Rule
ruleCheckSign = Rule "check-signatures"
                "top declaration has no corresponging type signature"
                checkSigs

ruleCheckIfs :: Rule
ruleCheckIfs = Rule "check-ifs"
               "nested if"
               checkIfs

ruleCheckReturns :: Rule
ruleCheckReturns = Rule "check-returns"
                   "useless return statement in do block"
                   checkReturns

ruleCheckDos :: Rule
ruleCheckDos = Rule "check-dos"
               "useless do"
               checkDos

ruleCheckGuards :: Rule
ruleCheckGuards = Rule "check-guards"
                  "guard should be pattern match"
                  checkGuards

ruleCheckLines :: Rule
ruleCheckLines = Rule "check-lines"
                 "functions should be less than 10 lines x 80 columns"
                 checkLines

---- RULES END ----

showLong :: Warn -> String
showLong = show

issues :: [(Issue, String, String)]
issues = [(BadIf, "C1", "nested IFs")
         ,(BadGuard, "C2", "guards should")
         ,(BadGuard, "D1", "guards should")
         ,(BadGuard, "D2", "guards should")
         ,(BadGuard, "F3", "guards should")
         ,(BadGuard, "F4", "guards should")
         ,(BadGuard, "T1", "guards should")
         ,(BadGuard, "E1", "guards should")
         ,(BadGuard, "P1", "guards should")
         ,(BadGuard, "XX", "guards should")]

-- | Retrives a tuple with the code and description of a coding
-- style issue.
getIssueDesc :: Issue -> (String, String)
getIssueDesc BadIf =            ("C1", "nested IFs")
getIssueDesc BadGuard =         ("C2", "guard should be a pattern")
getIssueDesc BadDo =            ("D1", "useless DO")
getIssueDesc BadReturn =        ("D2", "useless generator")
getIssueDesc LineTooLong =      ("F3", "line too long")
getIssueDesc FunctionTooBig =   ("F4", "function too big")
getIssueDesc (NoSig s) =        ("T1", s ++ " has no signature")
getIssueDesc (ForbiddenExt f) = ("E1", f ++ " file contains forbidden extension")
getIssueDesc (NotParsable f) =  ("P1", f ++ " file is not parsable")
getIssueDesc (Debug s) =        ("XX", s) -- DEBUG

-- | Retrives a list of issues code and descriptions.
getIssuesList :: [(String, String)]
getIssuesList = [getIssueDesc BadIf
                , getIssueDesc BadGuard
                , getIssueDesc BadDo
                , getIssueDesc BadReturn
                , getIssueDesc LineTooLong
                , getIssueDesc FunctionTooBig
                , getIssueDesc (NoSig "some function")
                , getIssueDesc (ForbiddenExt "some")
                , getIssueDesc (NotParsable "some")
                , getIssueDesc (Debug "debug")
                ]

class (Show a) => ShowOpt a where
  -- | Creates a vera compatible output of form:
  -- `<complete path>:<line>: <gravity>:<code>`
  showVera :: a -> String
  -- | Creates an Argo compatible output of form:
  -- `<complete path>:<line>:<code>`
  showArgo :: a -> String

instance ShowOpt Warn where
  showVera = show
  showArgo (Warn w (f, l) _) = f ++ ":" ++ show l ++ ":" ++ showArgo w

instance ShowOpt Issue where
  showVera i = let (idd, msg) = getIssueDesc i in idd ++ " # " ++ msg
  showArgo i = let (idd, _) = getIssueDesc i in idd

instance Show Issue where
  show i = let (idd, msg) = getIssueDesc i in idd ++ " # " ++ msg

instance Show Gravity where
  show Info = "INFO"
  show Minor = "MINOR"
  show Major = "MAJOR"

instance Show Warn where
  show (Warn w (f, l) g) = f ++ ":" ++ show l ++ ": " ++ show g ++ ":" ++ show w

instance Ord Warn where
  compare (Warn _ (s1,l1) _) (Warn _ (s2,l2) _) | s1 == s2 = compare l1 l2
                                            | otherwise = compare s1 s2
