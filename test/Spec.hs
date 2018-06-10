import Test.Hspec
import Cpf

main :: IO ()
main = hspec $ do
    describe "Cpf.valid" $ do
        it "checks if CPF is valid" $ do
                      Cpf.valid [1,1,1,1,1,1,1,1,1,1,1] `shouldBe` False
        it "checks if CPF is valid" $ do
                      Cpf.valid [1,2,3,4,5,6,7,8,9,1,2] `shouldBe` False
        it "checks if CPF is valid" $ do
                      Cpf.valid [] `shouldBe` False
        it "checks if CPF is valid" $ do
                      Cpf.valid [0] `shouldBe` False
        it "checks if CPF is valid" $ do
                      Cpf.valid [3,3,6,3,6,6,7,3,0,2,5] `shouldBe` True
    describe "Cpf.encode" $ do
        it "encodes CPF to a string" $ do
                      Cpf.encode [1,1,1,1,1,1,1,1,1,1,1] `shouldBe` "111.111.111-11"
        it "encodes CPF to a string" $ do
                      Cpf.encode[1,2,3,4,5,6,7,8,9,1,2] `shouldBe` "123.456.789-12"
        it "encodes CPF to a string" $ do
                      Cpf.encode[] `shouldBe` ""
        it "encodes CPF to a string" $ do
                      Cpf.encode[0] `shouldBe` ""
        it "encodes CPF to a string" $ do
                      Cpf.encode [3,3,6,3,6,6,7,3,0,2,5] `shouldBe` "336.366.730-25"
        it "encodes CPF to a string" $ do
                      Cpf.encode [3,3,6,3,6,6,7,3,0,2,5,1] `shouldBe` ""
        it "encodes CPF to a string" $ do
                      Cpf.encode [1..100] `shouldBe` ""
    describe "Cpf.decode" $ do
        it "decodes string to CPF" $ do
            Cpf.decode "336.366.730-25" `shouldBe` [3,3,6,3,6,6,7,3,0,2,5]
        it "decodes string to CPF" $ do
            Cpf.decode "111.111.111-11" `shouldBe` [1,1,1,1,1,1,1,1,1,1,1]
        it "decodes string to CPF" $ do
            Cpf.decode "" `shouldBe` []
