-- Implementation of Monte Carlo simulation using the Metropolis
-- method.
--
-- Alexander Smith <asmitl@gmu.edu>
-- 2022
module Metropolis where
import System.Random


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
    step' x (dx:dxrand) (p:prand) = x':(step' x' dxrand prand)
      where
        x' = transition' x dx p


-- Calculate the energy for each x position
energies energyFunc xs = map energyFunc xs


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
