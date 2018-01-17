{-# LANGUAGE ExistentialQuantification,MultiParamTypeClasses  #-}

{-

Theere are two levels of the game.
1. Outermost level. That's what the
game engine is running. There is no teleportaiton
or anything. Time runs like wall-clock.

2. Inner level. This is called the matrix.
It allows for teleportation and variation in space and time.

-}

type MatrixClockTime = Int
  {- Number of milliseconds since time 0. -}

type DPt = (Int,Int)
type DPair = (Int,Int)
type CPt = (Double,Double)

-- "infinity"
inf :: Num a => a
inf = 2000

data Player m =
  Player { plyPos :: CPt, plyInAir :: Bool }
  deriving (Show, Eq)

type MtxSignal a = MatrixClockTime -> a

{- static horizontal platforms only currently -}
data Platform = Platform {
  pltY :: Int
 ,pltXBounds :: DPair
 {-, Int-}
 } deriving (Show, Eq)

data Clock =
  Clock {
    pos :: DPt
   ,clockTime :: MtxSignal MatrixClockTime
 }
instance Show Clock where show = show . pos

-- rectangle shaped solid objects
-- (xBounds, yBounds)
type SolidObject = (DPair,DPair)

class Show m => Map m where
  lbBound :: m -> DPt
  rtBound :: m -> DPt
  platforms :: m -> [Platform]
  solidObjects :: m -> [SolidObject]
  {- no moving platforms yet -}
  clocks :: m -> [Clock] -- various clocks at various positions
  playerInit :: Player m
  {- similarity measure between two Matrix states. -}
  {- similarity has to be in [0,1] for valid inputs -}
  similarity :: MatrixSt m -> MatrixSt m -> Double
  mpShow :: m -> String
  mpShow = show
  {- CAN-DO: specialization at a concrete time -}


{- Have to have [0,1] ! -}
dist2DSim :: (Double,Double) -> (Double,Double) -> Double
dist2DSim (x1,y1) (x2,y2) =
  let euclDist = sqrt ((x2-x1)**2+(y2-y1)**2)
  in  exp (-euclDist)

dist1DSim :: Double -> Double -> Double
dist1DSim x1 x2 = exp (- abs(x2-x1))

data Map0 =
  Map0 MatrixClockTime
  {- For Map0, only MatrixClockTime is the
     discriminating feature (other than the player). -}
  deriving (Show, Eq)

instance Map Map0 where
  lbBound _ = (0,0)
  rtBound _ = (22,13)
  platforms _ =
    zipWith Platform
      [2    ,4     ,9     ,2    ]
      [(3,7),(6,11),(5,7),(16,18)]
  solidObjects _ = zipWith (,)
    [(-inf,0)  ,(-inf,inf),(13,15)   ,(22,inf) ,(15,inf),(-inf,inf)]
    [(-inf,inf),(-inf,0)  ,(-inf,inf),(-inf,inf),(7,inf),(13,inf)]
  clocks _ = map (\p -> Clock p id) [(3,4),(19,4)]
  playerInit = Player (1.5,0) False
  similarity
    (MatrixSt p1 (Map0 clk1))
    (MatrixSt p2 (Map0 clk2)) =
      let plySim   = dist2DSim (plyPos p1) (plyPos p2)
          clockSim = dist1DSim (fromIntegral clk1) (fromIntegral clk2)
      in  0.5 * plySim + 0.5 * clockSim
    {- treat player and clock similarity equal -}
    
data MatrixSt map = {- a state of the matrix -} 
  MatrixSt {
    mtxPlayer :: Player map
   ,mtxMapSt :: map
  }
{- Currently, player and map are treated differently ... -}

instance Show m => Show (MatrixSt m) where
  show (MatrixSt ply map) = "MatrixSt with (" ++ show ply ++ ") and ("++show map++")"

initMatrix :: MatrixSt Map0
initMatrix =
  let map0 = Map0 0
  in  MatrixSt playerInit map0
