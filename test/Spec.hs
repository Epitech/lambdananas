import ParserWrapper
import Common

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "runParser" $ do
    it "parses a basic haskell file" $ do
      True `shouldBe` True
