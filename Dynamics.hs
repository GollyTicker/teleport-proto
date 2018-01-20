
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
  0 < r < 1 -}
bla r = weightProb (\x -> exp (-(  abs(x-0)*r + abs(x-9)*(1-r)  ))) p0
p0 = createParticles  [9,9,1,1,1,1,1,1,3,3] [0..9::Double]

renderMatrix :: MatrixSt m -> a
renderMatrix = undefined
