import System.IO
import Data.Char
import Data.List

main = do
	putStrLn "Enter first matrix (put // to denote row breaks):"
	m1 <- getLine
	putStrLn "Enter second matrix (put // to denote row breaks):"
	m2 <- getLine
	putStr $ doMult m1 m2

doMult :: String -> String -> String
doMult x y = if (length (head (buildMatrix (getRowLength x) (parseMatrix x)))) /= (length (buildMatrix (getRowLength y) (parseMatrix y)))
		then "\nError: dimensions do not match\n"
		else "\nResult:\n" ++ (printMatrix $ matrixMult (buildMatrix (getRowLength x) (parseMatrix x)) (buildMatrix (getRowLength y) (parseMatrix y)))

printMatrix :: Show a => [[a]] -> [Char]
printMatrix [] = ""
printMatrix (x:xs) = printRow x ++ "\n" ++ printMatrix xs 

printRow :: Show a => [a] -> [Char]
printRow [] = ""
printRow (x:xs) = show x ++ " " ++ printRow xs

getRowLength :: String -> Int
getRowLength x = head $ findIndices (== "//") $ words x

parseMatrix :: (Num b, Read b) => String -> [b]
parseMatrix x = map (+0) $ map read $ filter (/= "//") $ words x

buildMatrix :: Num a => Int -> [a] -> [[a]]
buildMatrix _ [] = []
buildMatrix y x = take y x : buildMatrix y (drop y x)

matrixMult :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMult x y = matrixMultT x $ transpose y

matrixMultT :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultT [] _ = []
matrixMultT (a:as) b = calcRow a b : matrixMultT as b

calcRow :: Num a => [a] -> [[a]] -> [a]
calcRow _ [] = []
calcRow a (b:bs) = calcCell a b : calcRow a bs

calcCell :: Num a => [a] -> [a] -> a
calcCell col row = foldl1 (+) $ zipWith (*) col row
