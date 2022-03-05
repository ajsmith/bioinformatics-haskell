module MetropolisSpec where
import Test.Hspec
import Numeric
import System.Random (mkStdGen)
import Metropolis

spec :: Spec
spec = do
  describe "Metropolis.energy" $ do

    it "is symmetric around x = 0" $ do
      (energy 0) `shouldBe` (0 :: Double)
      (energy (-1) == energy 1) `shouldBe` True
      (energy (-2) == energy 2) `shouldBe` True
      (energy (-10) == energy 10) `shouldBe` True
      (energy (-999) == energy 999) `shouldBe` True
      (energy 0 == energy 1) `shouldBe` False

    it "increases as x moves away from 0" $ do
      (energy 0 < energy 1) `shouldBe` True
      (energy 1 < energy 2) `shouldBe` True
      (energy 2 < energy 10) `shouldBe` True

  describe "Metropolis.transition" $ do
    let transition' = transition energy

    it "always chooses new when E(new) <= E(old)" $ do
      (transition' 1 1 (-1) 1.0) `shouldBe` (0 :: Double)

    it "chooses new when E(n) > E(o) and random < transition probability" $ do
      (transition' 1 1 1 0) `shouldBe` (2 :: Double)

    it "choose old when E(n) > E(o) and rand >= transition probability" $ do
      let x = transitionProbability (energy 1) (energy (1 + 1)) 1
      (transition' 1 1 1 1) `shouldBe` (1 :: Double)
      (transition' 1 1 1 x) `shouldBe` (1 :: Double)

  describe "Metropolis.step" $ do
    let
      dxrand = dxrandInit (mkStdGen 571)
      prand = prandInit (mkStdGen 703)

    it "produces a list of new positions for x using MC Metropolis method" $ do
      let
        n = 100
        xInit = 0
        temp = 1
        xs = take n $ step energy temp xInit dxrand prand
        eAvg = averageEnergy energy xs
        eAvgErr = approximateError energy temp xs
      (showFFloat (Just 3) eAvg "") `shouldBe` "0.534"
      ((abs eAvgErr) < 0.1) `shouldBe` True


  describe "Metropolis.Metropolis" $ do
    let
      temp = 1
      x0 = 100
      delta = 10

    it "can produce a trivial simulation of only the initial values" $ do
      let
        n = 1
        rands = []
        sim = Metropolis energy temp x0 delta n rands
      (positions sim) `shouldBe` [100]
      (energies' sim) `shouldBe` [1000]
      (averageEnergy' sim) `shouldBe` 1000

    it "can be used for all Metropolis conditions" $ do
      let
        n = 4
        rands = [
          0, 0, -- Next E is lower, so it's automatically taken.
          0.7, 0.9, -- Next E is higher, and the roll fails
          0.6, 0.0000000001 -- Next E is higher, and the roll passes
          ]
        sim = Metropolis energy temp x0 delta n rands
      (positions sim) `shouldBe` [100, 95, 95, 96]
      (energies' sim) `shouldBe` [1000, 902.5, 902.5, 921.6]
      (averageEnergy' sim) `shouldBe` 931.65
