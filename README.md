# bioinformatics-haskell

Implementations of bioinformatics algorithms in Haskell


# Requirements

- Glasgow Haskell Compiler (GHC)
- Cabal


# Project setup

1. Clone this repo
2. Run `cabal update`


# Running tests

Switch to the project directory and run `cabal test`. Any test
depdendencies should be installed automatically.


# Running programs

## Metropolis Monte Carlo

This program will run a default set of experiments from temperature
T=0.1 to T=1 with a step of 0.1.

Run using `cabal run mc-metropolis`


# Installation

Run `cabal install --overwrite-policy=always`


# Author

Alexander Smith

Copyright 2022
