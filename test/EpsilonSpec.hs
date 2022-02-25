module EpsilonSpec where
import Test.Hspec
import Epsilon

spec :: Spec
spec = do
  describe "Epsilon.epsilon" $ do

    it "is greater than zero" $ do
      epsilon > 0 `shouldBe` True

    it "truncates when divided by 2 and added to 1" $ do
      (epsilon / 2 + 1) `shouldBe` (1.0 :: Double)

  describe "Epsilon.isEpsilon" $ do

    it "is False for zero" $ do
      (isEpsilon 0) `shouldBe` False

    it "is False for negative numbers" $ do
      (is Epsilon (-1)) `shouldBe` False

    it "is False for numbers bigger than epsilon" $ do
      (isEpsilon (2 * epsilon)) `shouldBe` False

    it "is True for epsilon" $ do
      (is Epsilon) `shouldBe` False
