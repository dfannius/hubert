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
weightedPCs = zip [0..11] [100, 20, 80, 70, 70, 60, 30, 80, 40, 50, 50, 40]

-- Given a source pitch and an interval delta d, what's the weight of the result?
-- Uses both weightedIntervals and weightedPCs.
pitchWeight :: Int -> Int -> Float
pitchWeight p d = (fromJust $ lookup ((p + d) `mod` 12) weightedPCs) * (fromJust $ lookup d weightedIntervals)

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
noteStream s dur g = line $ map (\p -> note dur $ pitch p) $ pitchStream s g

-- Creates an infinite stream of pitches, using both intervals and PCs as input
pitchStream2 :: RandomGen g => Int -> g -> [Int]
pitchStream2 s g =
    let rs = randomRs (0.0, 1.0) g 
        f p r = p + randFromWeightedList (intervalWeights p) r
    in scanl f s rs
        
-- Converts pitchStream2 to a Music Pitch
noteStream2 :: RandomGen g => Int -> Dur -> g -> Music Pitch
noteStream2 s dur g = line $ map (\p -> note dur $ pitch p) $ pitchStream2 s g

