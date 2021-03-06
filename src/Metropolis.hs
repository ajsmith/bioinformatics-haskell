-- Implementation of Monte Carlo simulation using the Metropolis
-- method.
--
-- Alexander Smith <asmitl@gmu.edu>
--
module Metropolis where
import System.Random
import Text.Printf
import Control.Monad


-- An energy function of x
energy x = k * x**2
  where
    k = 0.1


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


-- Run a set of Metropolis Monte Carlo experiments and print the
-- results to STDOUT as a simple table.
main :: IO ()
main = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  let
    n = 1000000
    temperatures = [0.1, 0.2 .. 1]
    experiments = [experiment gen1 gen2 n t | t<-temperatures]
  printExperimentTable experiments


-- Perform a Metropolis MC experiment for given N and T parameters.
-- Random number generators, gen1 and gen2, are used to generate
-- random moves for dx and random trial attempts.
experiment gen1 gen2 n temperature = (n, temperature, eAvg, approxErr)
  where
    (x0:dxrand) = dxrandInit gen1
    prand = prandInit gen2
    xs = take n $ step energy temperature x0 dxrand prand
    es = energies energy xs
    eAvg = averageEnergy energy xs
    approxErr = approximateError energy temperature xs


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
