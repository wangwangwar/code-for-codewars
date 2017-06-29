{-# LANGUAGE ScopedTypeVariables #-}

import           HuffmanEncoding
import           Test.Hspec

main = hspec $ do

    describe "frequencies" $ do

        it "return correct frequencies" $ do
            let fs = frequencies "aaaabcc"
            fs `shouldBe` [('a', 4), ('b', 1), ('c', 2)]

            let fs2 = frequencies "aaacbbaca"
            fs2 `shouldBe` [('a', 5), ('b', 2), ('c', 2)]

    describe "insertTree" $ do

        it "insert a tree" $ do
            let trees = fsToTrees [('a', 1), ('b', 2), ('c', 2)]
            let node = Leaf 'x' 2
            insertTree node trees `shouldBe` [Leaf 'a' 1, Leaf 'x' 2, Leaf 'b' 2, Leaf 'c' 2]

    describe "buildTree" $ do

        it "return a tree" $ do
            let fs = [('b', 2), ('c', 2), ('a', 5)]
            (buildTree . fsToTrees) fs `shouldBe` Node (Node (Leaf 'b' 2) (Leaf 'c' 2) 4) (Leaf 'a' 5) 9

        it "fsToCoding" $ do
            let fs = frequencies "aaaabcc"
            let codings = fsToCoding fs
            codings `shouldBe` [('b',[Z,Z]),('c',[Z,O]),('a',[O])]

    describe "basic tests" $ let fs = frequencies "aaaabcc" in do
        it "aaaabcc encoded should have length 10" $
            fmap length (encode fs "aaaabcc") `shouldBe` Just 10
        it "empty list encode" $
            encode fs [] `shouldBe` Just []
        it "empty list decode" $
            decode fs [] `shouldBe` Just []

    describe "error handling" $ do
        it "empty frequencies encode 1" $ encode [] "abc" `shouldBe` Nothing
        it "empty frequencies encode 2" $ encode [] "" `shouldBe` Nothing
        it "singleton frequency encode 1" $ encode [('a', 1)] "a" `shouldBe` Nothing
        it "singleton frequency encode 2" $ encode [('a', 1)] "" `shouldBe` Nothing

        it "empty frequencies decode 1" $ (decode [] [Z, O] :: Maybe String) `shouldBe` Nothing
        it "empty frequencies decode 2" $ (decode [] [] :: Maybe String) `shouldBe` Nothing
        it "singleton frequency decode 1" $ decode [('a', 1)] [Z, O] `shouldBe` Nothing
        it "singleton frequency decode 2" $ decode [('a', 1)] [] `shouldBe` Nothing

    describe "inversible" $ do

        it "should inversible" $ do
            let str = "o~\DLE"
            let fs = [('\DLE',1),('o',1),('~',1)]
            let coding = fsToCoding fs
            --coding `shouldBe` []
            let x = findBits coding [O, O, Z, O, Z]
            x `shouldBe` Just ('o', [O, O])

            encode fs str `shouldBe` Just [O,O,Z,O,Z]
            decode fs [O,O,Z,O,Z] `shouldBe` Just str

            let str2 = "\177<-*\FS8D!D\174AMY 7\CAN{6\192"
            let fs2 = [('\CAN',1),('\FS',1),(' ',1),('!',1),('*',1),('-',1),('6',1),('7',1),('8',1),('<',1),('A',1),('D',2),('M',1),('Y',1),('{',1),('\174',1),('\177',1),('\192',1)]
            encode fs2 str2 `shouldBe` Just [O,O,Z,O,O,Z,O,Z,O,Z,Z,Z,O,Z,Z,Z,Z,O,O,O,Z,O,Z,O,Z,Z,O,O,O,O,Z,Z,O,O,O,O,O,O,O,O,Z,O,Z,O,Z,O,Z,O,Z,O,O,O,Z,Z,Z,Z,Z,O,Z,Z,O,O,O,O,O,O,Z,Z,O,Z,Z,O,Z,O,O,Z,O,O,Z,Z]
            decode fs2  [O,O,Z,O,O,Z,O,Z,O,Z,Z,Z,O,Z,Z,Z,Z,O,O,O,Z,O,Z,O,Z,Z,O,O,O,O,Z,Z,O,O,O,O,O,O,O,O,Z,O,Z,O,Z,O,Z,O,Z,O,O,O,Z,Z,Z,Z,Z,O,Z,Z,O,O,O,O,O,O,Z,Z,O,Z,Z,O,Z,O,O,Z,O,O,Z,Z] `shouldBe` Just str2
