module Lib
    ( someFunc
    ) where
import Data.Array

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fibs :: Int -> Array Int Int
fibs n = a where
        a = array (0,n) (
            [(0,1),(1,1)]++
            [(i,a!(i-2)+a!(i-1))| i <- [2..n]] )

array2d = array ((1,1),(9,9)) [((a, b), a ^ 2 + b ^ 2) | a <- [1..9], b<-[1..9]]

myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [x] = x
myUnwords (x:xs) = x ++ "\U2503" ++ myUnwords xs

showTable arr = 
  unlines $ map (myUnwords . map (show . (arr !))) indices
  where indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]]
        ((startX, startY), (endX, endY)) = bounds arr