import Test.Hspec
import Cpf

main :: IO ()
main = hspec $ do
    describe "Cpf.valid" $ do
        it "checks if CPF is valid" $ do
                     Cpf.valid [1,1,1,1,1,1,1,1,1,1,1] `shouldBe` False
    describe "Cpf.valid" $ do
        it "checks if CPF is valid" $ do
                     Cpf.valid [8,8,6,8,1,8,9,2,1,6,9] `shouldBe` True
