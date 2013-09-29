import Data.Maybe
import Euterpea
import System.Random

-- Choose a random element from a weighted list, given a random number from 0 to 1
randFromWeightedList :: [(a, Float)] -> Float -> a
randFromWeightedList vs r =
    let totalWeight = sum $ map snd vs
        f a ((x,v):xs) = if (a < v) then x else f (a - v) xs
    in f (r * totalWeight) vs

type WeightedList a = [(a, Float)]

-- How likely we are to change pitch by a particular interval
weightedIntervals :: WeightedList Int
weightedIntervals = [( 0,  50),
                     ( 1, 300),
                     ( 2, 400),
                     ( 3, 200),
                     ( 4, 100),
                     ( 5, 100),
                     ( 6,  50),
                     (-1, 300),
                     (-2, 400),
                     (-3, 200),
                     (-4, 100),
                     (-5, 100),
                     (-6,  50)]

-- How likely we are to pick a particular pitch class for our next note
weightedPCs :: WeightedList Int
weightedPCs = zip [0..11] [200, 20, 180, 170, 170, 160, 30, 180, 40, 150, 50, 140]

weightedDurs = [(sn, 100), (en, 200), (qn, 50)]

-- Given a source pitch and an interval delta d, what's the weight of the result?
-- Uses both weightedIntervals and weightedPCs.
pitchWeight :: Int -> Int -> Float
pitchWeight p d = 
    intWt * pcWt
    where intWt = fromJust $ lookup d weightedIntervals
          pcWt  = fromJust $ lookup ((p + d) `mod` 12) weightedPCs

-- Computes a weighted list of intervals given the current pitch
intervalWeights :: Int -> [(Int, Float)]
intervalWeights p = map (\d -> (d, pitchWeight p d)) [-6..6]

-- Creates an infinite stream of intervals, ignoring PCs
intervalStream :: RandomGen g => g -> [Int]
intervalStream g = map (randFromWeightedList weightedIntervals) (randomRs (0.0, 1.0) g)

-- Creates an infinite stream of pitches, ignoring PCs
pitchStream :: RandomGen g => Int -> g -> [Int]
pitchStream s g = scanl (+) s $ intervalStream g

-- Converts pitchStream to a Music Pitch
noteStream :: RandomGen g => Int -> Dur -> g -> Music Pitch
noteStream s dur g = line $ map (note dur . pitch) $ pitchStream s g

-- Creates an infinite stream of pitches, using both intervals and PCs as input
pitchStream2 :: RandomGen g => Int -> g -> [Int]
pitchStream2 s g =
    let rs = randomRs (0.0, 1.0) g 
        f p r = p + randFromWeightedList (intervalWeights p) r
    in scanl f s rs
        
-- Converts pitchStream2 to a Music Pitch
noteStream2 :: RandomGen g => Int -> Dur -> g -> Music Pitch
noteStream2 s dur g = line $ map (note dur . pitch) $ pitchStream2 s g

durStream :: RandomGen g => g -> [Dur]
durStream g = map (randFromWeightedList weightedDurs) (randomRs (0.0, 1.0) g)

noteStream3 :: RandomGen g => Int -> g -> Music Pitch
noteStream3 s g = line $ map (\(p,d) -> note d $ pitch p) $ zip (pitchStream2 s g1) (durStream g2)
                  where (g1, g2) = split g

testIt x = play $ takeM (8 * wn) $ chord $ map (\(p,g,t) -> tempo t $ noteStream3 p (mkStdGen g)) $ zip3 [48,60,72] [x..] [0.3,0.56,0.87]
