module Main where

import Data.List
import System.IO
import Text.Printf

main :: IO ()
main =  hSetBuffering stdout NoBuffering
     >> interact grading

grading :: String -> String
grading =  promptResult . gradeGathering . divideIntoInts

divideIntoInts :: String -> [Int] 
divideIntoInts =  takeWhile (0 <) . map read . lines

data Grade = A | B | C | D deriving (Show)
type Stat  = (Int, Int, Int, Int)

gradeGathering :: [Int] -> (Stat, [Grade])
gradeGathering = mapAccumL gradeGather (0,0,0,0)

gradeGather :: Stat -> Int -> (Stat, Grade)
gradeGather stat score = (upd g stat, g)
  where
    upd A (a,b,c,d) = (a+1,b,c,d)
    upd B (a,b,c,d) = (a,b+1,c,d)
    upd C (a,b,c,d) = (a,b,c+1,d)
    upd D (a,b,c,d) = (a,b,c,d+1)
    g | score < 50 = D
      | score < 65 = C
      | score < 80 = B
      | otherwise  = A

promptResult :: (Stat, [Grade]) -> String
promptResult ~(stat, gs)
  = "Score? " ++ concatMap prompt gs ++ pprStat stat

prompt :: Grade -> String
prompt g = show g ++ "\nScore? "

pprStat :: Stat -> String
pprStat (a,b,c,d)
  =  intercalate ", " (map ppr (zip [A,B,C,D] [a,b,c,d])) ++ "\n"
  
ppr :: (Grade, Int) -> String
ppr (g, c) = show g ++ ": " ++ show c

main' :: IO ()
main' = do
  { hSetBuffering stdout NoBuffering;
    putStr "Score? ";
    (a,b,c,d) <- loop (0,0,0,0);
    printf "A: %d, B: %d, C: %d, D: %d\n" a b c d 
  }

loop :: Stat -> IO Stat
loop (a,b,c,d) = do {
  input <- getLine;
  let score = (read input :: Int) in
  case score of
    _ | score < 50 -> do
          { print D; loop (a,b,c,d+1) }
      | score < 50 -> do
          { print C; loop (a,b,c+1,d) }
      | score < 50 -> do
          { print B; loop (a,b+1,c,d) }
      | otherwise  -> do
          { print A; loop (a+1,b,c,d) }
  }
