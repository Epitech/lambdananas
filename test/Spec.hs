import Test.Hspec
import Parser
import Rules
import Language.Haskell.Exts.Syntax
import Control.Monad

appliedTo :: Check -> [String] -> Either IOError [Warn]
appliedTo rule src = rule <$> parseHS "." (unlines src)

main :: IO ()
main = hspec $ do
  describe "parseHS" $
    it "parse haskell source" $
      let res = map void <$> parseHS "." "v = 42"
      in res `shouldBe` Right [PatBind () (PVar () (Ident () "v"))
                               (UnGuardedRhs () (Lit () (Int () 42 "42")))
                               Nothing]

  describe "checkSigs" $ do
    it "should detect a function with no signature" $
      checkSigs
      `appliedTo` ["f x = 42"]
      `shouldBe` Right [Warn (NoSig "f") (".", 1)]
      
    it "should stay silent if all functions have signature" $
      checkSigs
      `appliedTo` ["f :: Int -> Int",
                   "f x = 42"]
      `shouldBe` Right []
      
    it "should work on multiple functions" $
      checkSigs
      `appliedTo` [ "f1 :: Int -> Int",
                    "f1 x = 42",
                    "",
                    "f2 x y = x + y",
                    "",
                    "f3 toto = 42",
                    "",
                    "a :: Int",
                    "a = 42" ]
      `shouldBe` Right [Warn (NoSig "f2") (".", 4),
                        Warn (NoSig "f3") (".", 6)]

  describe "checkIfs" $ do
    it "should detect nested ifs" $
      checkIfs
      `appliedTo` ["f x = if x > 10",
                   "      then if x < 100",
                   "          then True",
                   "          else False",
                   "     else False"]
      `shouldBe` Right [Warn BadIf (".", 1)]

    it "should be silent if only one if" $
      checkIfs
      `appliedTo` ["f x = if x > 10",
                   "      then True",
                   "      else False"]
      `shouldBe` Right []

    it "should detect nested ifs even if nested in parenthesis" $
      checkIfs
      `appliedTo` ["f x = if x > 10",
                   "      then (if x < 100",
                   "            then True",
                   "            else False)",
                   "     else False"]
      `shouldBe` Right [Warn BadIf (".", 1)]

  describe "checkDos" $ do
    it "should detect useless usage of do" $
      checkDos
      `appliedTo` ["f x y = do",
                   "        let a = x * x",
                   "        let b = y * y",
                   "        a + b"]
      `shouldBe` Right [Warn BadDo (".", 1)]

    it "should stay silent on allowed usage of do" $
      checkDos
      `appliedTo` ["f = do",
                   "    a <- getLine",
                   "    b <- getLine",
                   "    return a + b"]
      `shouldBe` Right []

    it "should detect bad returns" $
      checkDos
      `appliedTo` ["f x y = do",
                   "    a <- return (x * x)",
                   "    b <- return (y * y)",
                   "    return a + b"]
      `shouldBe` Right [Warn BadDo (".", 1)]

  describe "checkReturns" $ do
    it "should detect bad returns" $
      checkReturns
      `appliedTo` ["f x y = do",
                   "    a <- return (x * x)", 
                   "    b <- return (y * y)",
                   "    return a + b"]
      `shouldBe` Right [Warn BadReturn (".", 2),
                        Warn BadReturn (".", 3)]
      
    it "should stay silent on good generators and returns" $
      checkReturns
      `appliedTo` ["f = do",
                   "    a <- getLine",
                   "    b <- getLine",
                   "    return a + b"]
      `shouldBe` Right []

  describe "checkGuards" $ do
    it "should detect guards which should be pattern matches" $
      checkGuards
      `appliedTo` ["f x | x == 0 = True",
                   "    | 1 == x = False",
                   "    | otherwise = True"]
      `shouldBe` Right [Warn BadGuard (".", 1),
                        Warn BadGuard (".", 2)]
      
    it "should stay silent on good guards" $
      checkGuards
      `appliedTo` ["f x | null x = True",
                   "    | length x == 1 = False",
                   "    | otherwise = True"]
      `shouldBe` Right []

    it "should work on constructors" $
      checkGuards
      `appliedTo` ["f (x:xs) | xs == [] = True",
                   "         | Just 1 == x = False",
                   "         | otherwise = True"]
      `shouldBe` Right [Warn BadGuard (".", 1),
                        Warn BadGuard (".", 2)]
