
module Dynamics where

import Base
import ProbBase

data MatrixSt m =
  MatrixSt { getParticles :: (Particles m) {- + music state etc. -}}
  deriving (Show, Eq)


{- simulate t milliseconds in matrix.
will be called from the game system -}
simulate :: Double -> MatrixSt m -> MatrixSt m
simulate t mtx = undefined


{- potential balancing function between visualization and stabilization?
  0 < r < 1.

This looks good :)  -}
bla r = weightProb (\x -> r*exp (-abs(x-0)) + (1-r)*exp ( -abs(x-9)) ) p0
p0 = createParticles  [9,9,1,1,1,1,1,1,3,3] [0..9::Double]

renderMatrix :: MatrixSt m -> a
renderMatrix = undefined
