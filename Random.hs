import System.Random

seed:: Int 
seed = 40
giveList :: [Int]
giveList = [8,9,4,5,2]
gen = mkStdGen seed

giveRandomElement :: Int 
giveRandomElement = giveList !! rand where
    n = length giveList
    (rand, _) = randomR (0, (n-1)) gen
