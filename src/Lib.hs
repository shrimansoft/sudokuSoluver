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

array2d = array ((1,1),(9,9)) [((a, b), 0 ) | a <- [1..9], b<-[1..9]]


framing :: String -> String -> String -> String -> [String] -> String
framing initCh finalCh fillCh cutCh  [x1,x2,x3,x4,x5,x6,x7,x8,x9] = myFold [initCh,x1,fillCh,x2,fillCh,x3,cutCh,x4,fillCh,x5,fillCh,x6,cutCh,x7,fillCh,x8,fillCh,x9,finalCh] where
    myFold [] = ""
    myFold (x:xs) = x ++ myFold xs


showTable arr = 
  framingf $ map ((framing "┃ " " ┃\n" " │ " " ┃ " ) . map (myShow . (arr !))) indices
  where 
      framingf = framing "┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓\n" "┗━━━┷━━━┷━━━┻━━━┷━━━┷━━━┻━━━┷━━━┷━━━┛\n" "┠───┼───┼───╂───┼───┼───╂───┼───┼───┨\n" "┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫\n" 
      indices = [[(x, y) | x <- [startX..endX]] | y <- [startY..endY]]
      ((startX, startY), (endX, endY)) = bounds arr
      myShow n = if n == 0 then " " else show n