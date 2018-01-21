{-# LANGUAGE MultiParamTypeClasses #-}

module ProbBase
  (Particles, createParticles, uniform, weightProb, foldwp, resample,
    show', show'', p1,p2, wp1, wp2, main)
  where

import Text.Printf
import Data.List
import System.Random
import System.Random.MWC.Probability (samples, categorical,Gen)
import Control.Monad.Primitive (PrimState,PrimMonad)

data Particles a =
  Particles {
    probs :: [Double]
   ,particles :: [a]
} {- weights x values -}
  deriving (Show, Eq)

normalize :: [Double] -> [Double]
normalize ws = let z = sum ws in map (/z) ws

createParticles :: [Double] -> [a] -> Particles a
createParticles ws xs =
  if length ws /= length xs
    then error "Length mismatch: createParticles"
    else Particles (normalize ws) xs

{- Use show' and show'' to visualize particles -}
show' :: PrintfArg a => Particles a -> String
show' (Particles ws xs) = 
  "Particles:\n" ++
    intercalate "\n" (zipWith xwString xs ws)
  where
    xwString x w = printf "p(%v) = %.3f. " x w 
      ++ replicate (round (w*50)) '*'

show'' :: PrintfArg a => Particles a -> IO ()
show'' = putStrLn . show'

uniform :: [a] -> Particles a
uniform xs =
  let n = length xs
  in  Particles (replicate n (1.0 / fromIntegral n)) xs

weightProb :: (a -> Double) -> Particles a -> Particles a
weightProb f (Particles ws xs) =
  let ws' = zipWith (*) (map f xs) ws
  in  Particles (normalize ws') xs

foldwp :: [a -> Double] -> Particles a -> Particles a
foldwp fs p = foldr weightProb p fs


resample ::
  PrimMonad m => Gen (PrimState m) -> Int -> Particles a -> m (Particles a)
resample g n (Particles ws xs) =
  do  is <- samples n (categorical ws) g
      return $ uniform (map (xs!!) is)

{- CAN-DO: switch from lists to unboxed Vector for more efficiency
https://www.schoolofhaskell.com/user/commercial/content/vector
-}
{- Maybe for some special cases a analytic variant is feasible? -}

{- CAN-DO: use analytical probability distributions.
  allows for easier propagate and avoids systemic effects of resample.
    Maybe use ? https://hackage.haskell.org/package/random-fu-0.2.7.0/docs/Data-Random-Distribution.html#t:Distribution
    Or discrete distributions: https://hackage.haskell.org/package/Dist-0.4.1.0/docs/Numeric-Probability-Distribution.html
    Oh... Random Variables are cool!
    https://hackage.haskell.org/package/random-fu-0.2.7.0/docs/Data-Random-RVar.html#t:RVar    
-}
{- some examples -}

p1 = uniform [1,2,3,4,5::Double]
p2 = weightProb (\x -> exp (- abs (x-3))) p1

wp1 = weightProb (\x -> exp (- abs(x-2) * 3))
wp2 = weightProb (\x -> exp (- abs(x-5)))
testfold = foldwp
      [(\x -> exp (- abs(x-2) * 3)),
       (\x -> exp (- abs (x-5)))]

main = show'' $ testfold p1