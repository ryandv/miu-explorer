module MiuSystemSpec where

import Test.Hspec
import Miu.System

main :: IO ()
main = hspec $ do

  describe "miuRuleOne" $ do
    it "appends a U to strings ending in I" $ do
      miuRuleOne [M,I] `shouldBe` Just [M,I,U]

    it "returns Nothing for strings not ending in I" $ do
      miuRuleOne [M] `shouldBe` Nothing
      miuRuleOne [U] `shouldBe` Nothing

  describe "miuRuleTwo" $ do
    it "duplicates the substring following the initial letter M" $ do
      miuRuleTwo [M,I,U] `shouldBe` Just [M,I,U,I,U]
      miuRuleTwo [M,U,M] `shouldBe` Just [M,U,M,U,M]
      miuRuleTwo [M,U] `shouldBe` Just [M,U,U]

  describe "miuRuleThree" $ do
    it "replaces the first instance of III with U" $ do
      miuRuleThree [U,M,I,I,I,M,U] `shouldBe` Just [U,M,U,M,U]
      miuRuleThree [M,I,I,I,I] `shouldBe` Just [M,I,U]
