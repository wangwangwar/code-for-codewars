{-# LANGUAGE ScopedTypeVariables #-}

import           ChurchNumbers
import           Data.List       hiding (permutations)
import           Permutations
import           RangeExtraction
import           Test.Hspec
import           ValidBraces

import           Control.Monad
import           Data.Function
import           FixIt           (foldr', reverse')
import           Text.Printf


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

  describe "Testing Church" $ do

      it "Works for examples" $ do
        findChurch churchAdd 1 3 `shouldBe` 4
        findChurch churchAdd 1 0 `shouldBe` 1
        findChurch churchMul 1 3 `shouldBe` 3
        findChurch churchMul 0 3 `shouldBe` 0
        findChurch churchMul 2 3 `shouldBe` 6
        findChurch churchPow 1 3 `shouldBe` 1
        findChurch churchPow 2 3 `shouldBe` 8

  describe "" $ do

    let fixFoldr = fix foldr'
    let fixReverse = fix reverse'
    let input1 = [2,3,5,7,11]
    let expected1 = 28
    let expected2 = [11,7,5,3,2]
    let input4 = "Reverse"
    let expected4 = "esreveR"

    describe (show input1) $ do

        it (printf "reverse of %s is %s" (show expected2) (show input1)) $ do
          fixReverse input1 `shouldBe` expected2

    describe (show input4) $ do

        it (printf "reverse of %s is %s" (show expected4) (show input4)) $ do
          fixReverse input4 `shouldBe` expected4

    describe (show input1) $ do

        it (printf "sum of %s should return %s" (show expected1) (show input1)) $ do
          fixFoldr (+) 0 input1 `shouldBe` expected1
