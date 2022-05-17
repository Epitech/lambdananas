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

data Rule = Rule { name :: String
                 , _description :: String
                 , getRule :: Check
                 }

instance Eq Rule where
  r1 == r2 = name r1 == name r2

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

showLong :: Warn -> String
showLong = show

type Check = [Decl SrcSpanInfo] -> [Warn]

-- | A coding style warning emitted by the checker.
data Warn = Warn { what :: Issue                -- ^ The issue raised (description can be retrived by 'getIssueDesc')
                 , _location :: (FilePath, Int) -- ^ The location of the issue
                 , gravity :: Gravity           -- ^ The gravity of the issue
                 } deriving Eq

-- | All possible issues arising from a code.
data Issue = BadIf                        -- ^ Nested ifs
           | BadDo                        -- ^ Useless do
           | BadReturn                    -- ^ Useless generator
           | BadGuard                     -- ^ Guard should be a pattern
           | LineTooLong                  -- ^ Line too long
           | FunctionTooBig               -- ^ Function too big
           | NoSig String                 -- ^ No signature
           | NotParsable FilePath         -- ^ File is not parsable
           | ForbiddenExt FilePath        -- ^ File contains forbidden extension
           | Debug String                 -- ^ Debug
           deriving Eq

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
getIssueDesc (ForbiddenExt f) = ("P1", f ++ " file contains forbidden extension")
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
                , getIssueDesc (NotParsable "some")
                , getIssueDesc (Debug "debug")
                ]

-- | Describes an 'Issue' gravity.
data Gravity = Info | Minor | Major deriving Eq

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

{- CHECK CASCADING IFS -}
checkIfs :: Check
checkIfs = join . explore checkIf
  where checkIf (NExp (If ssi _ ift ife)) | countIfs ift ife >= 1 =
          [Warn BadIf (getLoc ssi) Major]
        checkIf _ = []
        countIfs ifthen ifelse = inspectExpr countIf ifthen <>
                                 inspectExpr countIf ifelse
        countIf :: Node -> Sum Int
        countIf (NExp If{}) = Sum 1
        countIf _ = Sum 0

{- CHECK USELESS DOs -}
checkDos :: Check
checkDos = join . explore checkDo
  where checkDo (NExp (Do ssi body)) | countGenerators body < 1 =
                                       [Warn BadDo (getLoc ssi) Major]
        checkDo _ = []
        countGenerators = foldMap (countGenerator . NSmt)
        countGenerator :: Node -> Sum Int
        countGenerator = checkGen (\ _ isRet ->
                                     if isRet then Sum 0 else Sum 1)

{- CHECK USELESS Returns -}
checkReturns :: Check
checkReturns = join . explore checkReturn
  where checkReturn (NExp (Do _ body)) =
          foldMap (badReturns . NSmt) body
        checkReturn _ = []
        badReturns = checkGen toWarn
        toWarn ssi True = [Warn BadReturn (getLoc ssi) Minor]
        toWarn _ _ = []

{- auxiliary functions for checkDos and checkReturns -}
checkGen :: Monoid m => (SrcSpanInfo -> Bool -> m) -> Node -> m
checkGen f (NSmt (Generator ssi _ e1)) = f ssi $ isReturn e1
checkGen _ _ = mempty

isReturn :: Exp SrcSpanInfo -> Bool
isReturn (App _ (Var _ (UnQual _ (Ident _ f))) _) = isRetOrPure f
isReturn (InfixApp _ (Var _ (UnQual _ (Ident _ f))) _ _) = isRetOrPure f
isReturn (Paren _ e) = isReturn e
isReturn _ = False

isRetOrPure :: String -> Bool
isRetOrPure "return" = True
isRetOrPure "pure" = True
isRetOrPure _ = False

{- CHECK IF ALL TOP DECLARATION HAS A TYPE SIGNATURE -}
checkSigs :: Check
checkSigs lst = join $ map genWarn binds
  where sigsAndBinds = explore collectSigs lst
        sigs = foldMap fst sigsAndBinds
        binds = foldMap getBind sigsAndBinds
        getBind (_,l) = if null l then [] else [head l]
        genWarn (fct, ssi) | fct `notElem` sigs =
                             [Warn (NoSig fct) (getLoc ssi) Minor]
        genWarn _ = []

collectSigs :: Node -> ([String], [(String, SrcSpanInfo)])
collectSigs (NDec (TypeSig _ (x:_) _ )) = ([getIdent x],[])
collectSigs (NDec (PatBind ssi (PVar _ idt) _ _ )) =
  ([],[(getIdent idt, ssi)])
collectSigs (NDec (FunBind ssi (Match _ idt _ _ _:_))) =
  ([],[(getIdent idt, ssi)])
collectSigs _ = ([],[])

{- CHECK BAD GUARDS -}
checkGuards :: Check
checkGuards lst = join $ explore checkGuard lst
  where checkGuard (NDec (FunBind _ m)) =
          fold $ zipWith toWarns (vars m) (matchs m)
        checkGuard _ = []
        vars match = map (inspectMatch collectVar) match
        matchs match = map (inspectMatch collectGuards) match
        collectVar (NPat (PVar _ idt)) = [getIdent idt]
        collectVar _ = []
        collectGuards (NSmt (Qualifier _ expr)) = [expr]
        collectGuards _ = []

toWarns :: [String] -> [Exp SrcSpanInfo] -> [Warn]
toWarns vars = foldMap (inspectExpr toWarn)
  where toWarn (NExp (InfixApp ssi e1 e2 e3))
          | isBadGuard vars e1 e2 e3 = [Warn BadGuard (getLoc ssi) Major]
        toWarn _ = []

isBadGuard :: [String] -> Exp SrcSpanInfo -> QOp SrcSpanInfo ->
              Exp SrcSpanInfo -> Bool
isBadGuard vars e1 e2 e3 | isVar e1 && isEq e2 && isLit e3 = True
                         | isLit e1 && isEq e2 && isVar e3 = True
  where isVar (Var _ (UnQual _ (Ident _ x))) | x `elem` vars = True
        isVar _ = False
        isEq (QVarOp _ (UnQual _ (Symbol _ "=="))) = True
        isEq _ = False
isBadGuard _ _ _ _ = False

isLit :: Exp SrcSpanInfo -> Bool
isLit Lit{} = True
isLit Con{} = True
isLit (App _ Con{} _) = True
isLit (List _ []) = True
isLit _ = False

{- CHECK LINES LENGTH AND FUNCTION SIZE -}
checkLines :: Check
checkLines lst = uniqWarn $ join $ explore checkLine lst
  where checkLine (NDec (FunBind _ matches)) = foldMap checkLine' matches
        checkLine (NDec decl@PatBind{}) = checkLine' decl
        checkLine _ = []
        checkLine' decl = uniqFunWarn $ foldMap toWarn decl
        toWarn ssi@(SrcSpanInfo (SrcSpan _f l1 _c1 l2 c2) _) =
          [Warn FunctionTooBig (getLoc ssi) Minor | l2-l1 >= 10]
          ++
          [Warn LineTooLong (getLoc ssi) Minor | l1==l2 && c2 > 80]

uniqFunWarn :: [Warn] -> [Warn]
uniqFunWarn [] = []
uniqFunWarn (w1@(Warn FunctionTooBig _ Info):xs)
  | FunctionTooBig `elem` map what xs = uniqFunWarn xs
  | otherwise = w1 : uniqFunWarn xs
uniqFunWarn (x:xs) = x:uniqFunWarn xs

uniqWarn :: [Warn] -> [Warn]
uniqWarn [] = []
uniqWarn (x:xs) | x `elem` xs  = uniqWarn xs
                | otherwise = x : uniqWarn xs
