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
    it "replaces instances of III with U" $ do
      miuRuleThree [U,M,I,I,I,M,U] `shouldBe` [[U,M,U,M,U]]

    it "returns a list of values if multiple different applications are possible" $ do
      miuRuleThree [M,I,I,I,I] `shouldBe` [[M,U,I], [M,I,U]]

    it "returns the empty list when no applications are possible" $ do
      miuRuleThree [M,M,M] `shouldBe` []

  describe "miuRuleFour" $ do
    it "removes instances of UU from a given string" $ do
      miuRuleFour [M,I,U,U,I,M] `shouldBe` [[M,I,I,M]]

    it "returns a list of values if multiple different applications are possible" $ do
      miuRuleFour [M,U,U,I,U,U,U,M] `shouldBe` [[M,I,U,U,U,M], [M,U,U,I,U,M], [M,U,U,I,U,M]]

    it "returns the empty list when no applications are possible" $ do
      miuRuleFour [M,M,M,M] `shouldBe` []
