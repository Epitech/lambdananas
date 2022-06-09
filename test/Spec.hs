import Parser
import Common
import qualified BadDo (check)
import qualified BadDoReturn (check)
import qualified BadGuard (check)
import qualified BadIf (check)
import qualified FunctionTooWideOrLarge (check)
import qualified NoSig (check)

import Test.Hspec
import Control.Monad

appliedTo :: Check -> [String] -> Either ParseError [Warn]
appliedTo rule src = rule <$> parseHS "." (unlines src)

takeFirst :: (a, b, c) -> a
takeFirst (a, _, _) = a

main :: IO ()
main = hspec $ do
  describe "parseHS" $
    it "parse haskell source" $
      let res = map void <$> takeFirst <$> parseHS "." "v = 42"
      in res `shouldBe` Right [PatBind () (PVar () (Ident () "v"))
                               (UnGuardedRhs () (Lit () (Int () 42 "42")))
                               Nothing]

  describe "checkSigs" $ do
    it "should detect a function with no signature" $
      NoSig.check
      `appliedTo` ["f x = 42"]
      `shouldBe` Right [Warn NoSig (".", 1) (StringArg "f")]

    it "should stay silent if all functions have signature" $
      NoSig.check
      `appliedTo` ["f :: Int -> Int",
                   "f x = 42"]
      `shouldBe` Right []

    it "should work on multiple functions" $
      NoSig.check
      `appliedTo` [ "f1 :: Int -> Int",
                    "f1 x = 42",
                    "",
                    "f2 x y = x + y",
                    "",
                    "f3 toto = 42",
                    "",
                    "a :: Int",
                    "a = 42" ]
      `shouldBe` Right [Warn NoSig (".", 4) (StringArg "f2"),
                        Warn NoSig (".", 6) (StringArg "f3")]

  describe "checkIfs" $ do
    it "should detect nested ifs" $
      BadIf.check
      `appliedTo` ["f x = if x > 10",
                   "      then if x < 100",
                   "          then True",
                   "          else False",
                   "     else False"]
      `shouldBe` Right [Warn BadIf (".", 1) NoArg]

    it "should be silent if only one if" $
      BadIf.check
      `appliedTo` ["f x = if x > 10",
                   "      then True",
                   "      else False"]
      `shouldBe` Right []

    it "should detect nested ifs even if nested in parenthesis" $
      BadIf.check
      `appliedTo` ["f x = if x > 10",
                   "      then (if x < 100",
                   "            then True",
                   "            else False)",
                   "     else False"]
      `shouldBe` Right [Warn BadIf (".", 1) NoArg]

  describe "checkDos" $ do
    it "should detect useless usage of do" $
      BadDo.check
      `appliedTo` ["f x y = do",
                   "        let a = x * x",
                   "        let b = y * y",
                   "        a + b"]
      `shouldBe` Right [Warn BadDo (".", 1) NoArg]

    it "should stay silent on allowed usage of do" $
      BadDo.check
      `appliedTo` ["f = do",
                   "    a <- getLine",
                   "    b <- getLine",
                   "    return a + b"]
      `shouldBe` Right []

    it "should detect bad returns" $
      BadDo.check
      `appliedTo` ["f x y = do",
                   "    a <- return (x * x)",
                   "    b <- return (y * y)",
                   "    return a + b"]
      `shouldBe` Right [Warn BadDo (".", 1) NoArg]

  describe "checkReturns" $ do
    it "should detect bad returns" $
      BadDoReturn.check
      `appliedTo` ["f x y = do",
                   "    a <- return (x * x)",
                   "    b <- return (y * y)",
                   "    return a + b"]
      `shouldBe` Right [Warn BadReturn (".", 2) NoArg,
                        Warn BadReturn (".", 3) NoArg]

    it "should stay silent on good generators and returns" $
      BadDoReturn.check
      `appliedTo` ["f = do",
                   "    a <- getLine",
                   "    b <- getLine",
                   "    return a + b"]
      `shouldBe` Right []

  describe "checkGuards" $ do
    it "should detect guards which should be pattern matches" $
      BadGuard.check
      `appliedTo` ["f x | x == 0 = True",
                   "    | 1 == x = False",
                   "    | otherwise = True"]
      `shouldBe` Right [Warn BadGuard (".", 1) NoArg,
                        Warn BadGuard (".", 2) NoArg]

    it "should stay silent on good guards" $
      BadGuard.check
      `appliedTo` ["f x | null x = True",
                   "    | length x == 1 = False",
                   "    | otherwise = True"]
      `shouldBe` Right []

    it "should work on constructors" $
      BadGuard.check
      `appliedTo` ["f (x:xs) | xs == [] = True",
                   "         | Just 1 == x = False",
                   "         | otherwise = True"]
      `shouldBe` Right [Warn BadGuard (".", 1) NoArg,
                        Warn BadGuard (".", 2) NoArg]

  describe "checkLines" $ do
    it "should detect too long functions" $
      FunctionTooWideOrLarge.check
      `appliedTo` (["f 0 = "] ++ ["" | _ <- [1..10] ] ++ ["    True"])
      `shouldBe` Right [Warn FunctionTooBig (".", 1) NoArg]

    it "should detect too long lines" $
      FunctionTooWideOrLarge.check
      `appliedTo` ["f 0 = " ++ [' ' | _ <- [1..70] ] ++ "    True"]
      `shouldBe` Right [Warn LineTooLong (".", 1) NoArg]
