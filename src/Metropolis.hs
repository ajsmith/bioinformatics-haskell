-- Implementation of Monte Carlo simulation using the Metropolis
-- method.
--
-- Alexander Smith <asmitl@gmu.edu>
--
module Metropolis where
import System.Random
import Text.Printf
import Control.Monad


-- Data type for Metropolis Monte Carlo simulations
--
-- Metropolis constructor parameters:
--   energy function :: Real a => a -> a
--   temperature :: Real a => a
--   x start :: Real a => a
--   delta size :: Real a => a
--   step max :: Int
--   randoms :: [Double]
data (Real a) => Simulation a = Metropolis (a -> a) a a a Int [Double]


-- The energy function used in the simulation
energyFunction (Metropolis efun _ _ _ _ _) = efun


-- The temperature in the simulation
temperature (Metropolis _ t _ _ _ _) = t


-- The maximum number of steps in the simulation
stepMax (Metropolis _ _ _ _ stepMax' _) = stepMax'


-- The step delta of x in the simulation
delta (Metropolis _ _ _ delta' _ _) = delta'


-- The positions of x in the simulation
positions (Metropolis efun t x0 dx stepMax rand) = take stepMax positions'
  where
    positions' = steps efun t x0 dx rand


-- Recursion using the Metropolis method which builds an infinite list
-- of the positions of x
steps efun t x delta rs = x:(steps efun t x' delta rs')
  where
    x' = transition efun t x dx r2
    dx = delta * (r1 - 0.5)
    (r1:r2:rs') = rs


-- The energies corresponding to the x positions
energies' sim = map (energyFunction sim) (positions sim)


-- The average energy of the simulation
averageEnergy' sim = (sum (energies' sim)) / n
  where
    n = fromIntegral $ length $ energies' sim


-- The approximate error of the simulation
approximateError' sim = (2 * eAvg - t) / t
  where
    eAvg = averageEnergy' sim
    t = temperature sim


-- An energy function of x
energy x = k * x**2
  where
    k = 0.1


results sim = (n, t, eAvg, approxErr)
  where
    n = stepMax sim
    t = temperature sim
    eAvg = averageEnergy' sim
    approxErr = approximateError' sim


-- Initialize an infinite list of random changes to x
dxrandInit :: RandomGen g => g -> [Double]
dxrandInit = randomRs (-10, 10)


-- Initialize an infinite list of random trial attempts
prandInit :: RandomGen g => g -> [Double]
prandInit = randoms


-- Recursively perform Monte Carlo steps using the Metropolis method,
-- building an infinite list of subsequent steps
step energyFunc temperature x dxrand prand = step' x dxrand prand
  where
    transition' = transition energyFunc temperature
    step' x (dx:dxrand) (p:prand) = x:(step' x' dxrand prand)
      where
        x' = transition' x dx p


-- Calculate the energy for each x position
energies energyFunc = map energyFunc


-- Compute the average energy for a list of x positions
averageEnergy energyFunc xs = (sum (energies energyFunc xs)) / n
  where
    n = fromIntegral $ length xs


-- Estimate the error in the average energy for a list of x positions
approximateError energyFunc temperature xs = err
  where
    eAvg = averageEnergy energyFunc xs
    err = (2 * eAvg - temperature) / temperature


-- Transition to the next state using the Metropolis criterion
transition energyFunc temperature x dx p =
  if eNew <= eOld || p < transitionProbability'
  then x'
  else x
  where
    x' = x + dx
    eNew = energyFunc x'
    eOld = energyFunc x
    transitionProbability' = transitionProbability eNew eOld temperature


-- Calculate the trial probability from the difference in old and new
-- energies
transitionProbability eNew eOld temperature = exp ((-beta) * deltaE)
  where
    deltaE = eNew - eOld
    beta = 1 / temperature


newMain :: IO ()
newMain = do
  gen1 <- newStdGen
  let
    temperatures = [0.1, 0.2 .. 1]
    n = 100000
    x0 = 100
    delta = 10
    rs = take (2 * n) $ (randoms gen1 :: [Double])
    sims = [Metropolis energy t x0 delta n rs | t<-temperatures]
  printSimTable sims

-- Print a table of experiment results
printSimTable sims = do
  putStrLn "N\tT\t<E>\tError"
  mapM_ printSimRow sims


-- Print a table row of an experiment
printSimRow sim = do
  printf "%d\t%.2f\t%.3f\t%.3f\n" n t eAvg approxErr
  where
    (n, t, eAvg, approxErr) = results sim


-- Run a set of Metropolis Monte Carlo experiments and print the
-- results to STDOUT as a simple table.
oldMain :: IO ()
oldMain = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  let
    n = 100000
    temperatures = [0.1, 0.2 .. 1]
    experiments = [experiment gen1 gen2 n t | t<-temperatures]
  printExperimentTable experiments


-- Perform a Metropolis MC experiment for given N and T parameters.
-- Random number generators, gen1 and gen2, are used to generate
-- random moves for dx and random trial attempts.
experiment gen1 gen2 n temperature = (n, temperature, eAvg, approxErr)
  where
    x0 = 100
    dxrand = dxrandInit gen1
    prand = prandInit gen2
    xs = take n $ step energy temperature x0 dxrand prand
    es = energies energy xs
    eAvg = averageEnergy energy xs
    approxErr = approximateError energy temperature xs


acceptanceRate energyFun xs = acceptances' / (n - 1)
  where
    n = fromIntegral $ length xs
    acceptances' = acceptances energyFun xs


acceptances energyFun ([]) = 0
acceptances energyFun (_:[]) = 0
acceptances energyFun (x1:x2:xs) =
  if energyFun x2 > energyFun x1
  then 1 + (acceptances energyFun (x2:xs))
  else acceptances energyFun (x2:xs)


-- Print a table of experiment results
printExperimentTable experiments = do
  putStrLn "N\tT\t<E>\tError"
  mapM_ printExperimentRow experiments


-- Print a table row of an experiment
printExperimentRow (n, t, eAvg, approxErr) = do
  printf "%d\t%.2f\t%.3f\t%.3f\n" n t eAvg approxErr


-- Print an experiment somewhat verbosely
printExperiment (n, t, eAvg, approxErr) = do
  printf "N=%d T=%.2f <E>=%.3f Err=%.3f\n" n t eAvg approxErr


main = do
  newMain
  oldMain
