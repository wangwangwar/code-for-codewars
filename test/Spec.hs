{-# LANGUAGE ScopedTypeVariables #-}

import           ChurchNumbers
import           Data.List             hiding (permutations)
import           Permutations
import           RangeExtraction
import           Test.Hspec
import           ValidBraces

import           Control.Monad
import           Data.Function
import           FixIt                 (foldr', reverse')
import           Text.Printf

import           FiveFundamentalMonads

import           Control.Applicative
import           FunctionalStreams


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

  describe "Five Fundamental Monads" $ do

        it "works" $ do
          let a = State $ \s -> (0, s)
          print $ show $ runState a 1

  describe "Functional Streams" $ do

    let afterHeadS = headS . tailS

    describe "basics" $ do
        it "headS on Int" $ headS (42 :> undefined) `shouldBe` 42
        it "tail" $ afterHeadS ('a' :> 'b' :> undefined) `shouldBe` 'b'

    describe "peeking into streams" $ do
        it "takeS should work for negative indices" $ take 2 (takeS (-1) $ 1 :> 2 :> undefined) `shouldBe` []

    describe "stream constructors" $ do
        it "repeatS" $ headS (repeatS 42) `shouldBe` 42
        it "iterateS" $ afterHeadS (iterateS (+ 1) 0) `shouldBe` 1
        it "cycleS" $ afterHeadS (cycleS [1 .. 9]) `shouldBe` 2
        it "fromS" $ afterHeadS (fromS 42) `shouldBe` 43
        it "fromStepS" $ afterHeadS (fromStepS 42 2) `shouldBe` 44

    describe "general purpose functions" $ do
        it "foldrS" $ all (== 42) (take 10 $ foldrS (:) (repeatS 42)) `shouldBe` True
        it "filterS" $ takeS 4 (filterS even $ fromS 0) `shouldBe` [0, 2, 4, 6]
        it "takeS" $ length (takeS 5 $ repeatS 42) `shouldBe` 5
        it "takeWhileS" $ length (takeWhileS (< 5) $ fromS 0) `shouldBe` 5
        it "dropS" $ headS (dropS 10 $ fromS 0) `shouldBe` 10
        it "splitAtS" $ case splitAtS 1 (fromS 0) of { (ls, rs) -> (ls, headS rs) `shouldBe` ([0], 1) }
        it "zipWithS" $ headS (zipWithS (+) (repeatS 20) (repeatS 22)) `shouldBe` 42

    describe "class instances" $ do
        it "fmap" $ headS (fmap (+ 1) $ repeatS 1) `shouldBe` 2
        it "pure" $ takeS 2 (pure 42) `shouldBe` [42, 42]
        it "(<*>)" $ headS (pure (* 2) <*> pure 21) `shouldBe` 42

    describe "sequences" $ do
        it "fibonacci sequence" $ do
          print $ show $ takeS 10000 fibS
          takeS 4 fibS `shouldBe` [0, 1, 1, 2]
        it "prime sequence" $ do
          print $ show $ takeS 10000 $ _primeS 2 []
          takeS 10 primeS `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

