{-# LANGUAGE ScopedTypeVariables #-}

import           Data.List       hiding (permutations)
import           Permutations
import           RangeExtraction
import           Test.Hspec
import           ValidBraces

main = hspec $ do

  describe "Permutations" $ do

      it "return permutations" $ do
        permutations    "a" `shouldBe` ["a"]
        permutations   "ab" `shouldBe` ["ab", "ba"]
        permutations "aabb" `shouldBe` ["aabb","abab","abba","baab","baba","bbaa"]

  describe "Range extraction" $ do

      it "[-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]" $ do
        let grp = concatGrp [-6, -3, -2, -1, 0, 1, 3] [[]]
        print grp
        (rangeExtraction [-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]) `shouldBe` "-6,-3-1,3-5,7-11,14,15,17-20"


  describe "Valid Braces" $ do

      it "should work for some examples" $ do
        validBraces "()"             `shouldBe` True
        validBraces "[([)"           `shouldBe` False
        validBraces "())({}}{()][][" `shouldBe` False
        validBraces "({})[({})]"     `shouldBe` True
