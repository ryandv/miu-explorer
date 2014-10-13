module MiuUtilSpec where

import Miu.System
import Miu.Util

import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "kmpFailureFunction" $ do
    it "returns 0 for indices 0 and 1" $ do
      kmpFailureFunction 0 "foobar" `shouldBe` 0
      kmpFailureFunction 1 "foobaz" `shouldBe` 0

    it "returns the length of the longest proper suffix of a pattern that is also a prefix of the pattern" $ do
      kmpFailureFunction 2 "ABABAC" `shouldBe` 0
      kmpFailureFunction 3 "ABABAC" `shouldBe` 1
      kmpFailureFunction 4 "ABABAC" `shouldBe` 2
      kmpFailureFunction 5 "ABABAC" `shouldBe` 3
      kmpFailureFunction 6 "ABABAC" `shouldBe` 0

    it "passes this more complicated test" $ do
      kmpFailureFunction 0 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 1 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 2 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 3 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 4 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 5 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 6 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 7 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 8 "PARTICIPATE IN PARACHUTE" `shouldBe` 1
      kmpFailureFunction 9 "PARTICIPATE IN PARACHUTE" `shouldBe` 2
      kmpFailureFunction 10 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 11 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 12 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 13 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 14 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 15 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 16 "PARTICIPATE IN PARACHUTE" `shouldBe` 1
      kmpFailureFunction 17 "PARTICIPATE IN PARACHUTE" `shouldBe` 2
      kmpFailureFunction 18 "PARTICIPATE IN PARACHUTE" `shouldBe` 3
      kmpFailureFunction 19 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 20 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 21 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 22 "PARTICIPATE IN PARACHUTE" `shouldBe` 0
      kmpFailureFunction 23 "PARTICIPATE IN PARACHUTE" `shouldBe` 0

  describe "findSubstring" $ do
    it "returns the index of the requested substring if a match is found" $ do
      findSubstring [M] [M] `shouldBe` [0]
      findSubstring [U,U] [M,I,U,U,I,M] `shouldBe` [2]

    it "returns a list of indices if multiple matches are found" $ do
      findSubstring [I,I,I] [I,I,M,I,I,I,I,I,M,I,I,I] `shouldBe` [3,4,5,9]

    it "returns an empty list otherwise" $
      findSubstring [M,M,M] [U,U,U] `shouldBe` []
