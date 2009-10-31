module Main where

import Char  
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ord
import IO
import List

lowerWords :: String -> [ String ]
lowerWords = filter (not . null)
           . map (map toLower . filter isAlpha) 
           . words

train :: [ String ] -> Map.Map String Int
train = List.foldl' (\dict f -> Map.insertWith' (+) f 1 dict) Map.empty 

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

edits1 :: String -> [ String ]
edits1 word =
    let s = [ (take i word, drop i word) | i <- [ 0 .. length word ] ]
        deletes    = [ a ++ (tail b) | (a, b) <- s, not $ null b ]
        transposes = [ a ++ ((head $ tail b) : head b : drop 2 b) | (a, b) <- s, length b > 1 ]
        replaces   = [ a ++ (c : tail b) | (a, b) <- s, c <- alphabet, not $ null b ]
        inserts    = [ a ++ (c : b) | (a, b) <- s, c <- alphabet ]
    in nub (deletes ++ transposes ++ replaces ++ inserts)

known_edits2 :: Map.Map String Int -> String -> [ String ]
known_edits2 nwords word = [ e2 | e1 <- edits1 word, e2 <- edits1 e1, Map.member e1 nwords ]

known :: Map.Map String Int -> [ String ] -> [ String ]
known nwords = filter ((flip Map.member) nwords)

correct :: Map.Map String Int -> String -> String
correct nwords word = 
    let candidates = case known nwords [ word ] of
                          [ ] -> case known nwords $ edits1 word of
                                      [ ] -> case known_edits2 nwords word of
                                                  [ ] -> [ word ]
                                                  l -> l
                                      l -> l
                          l -> l
    in maximumBy (comparing (\c -> Map.findWithDefault undefined c nwords)) candidates

prompt :: Map.Map String Int -> IO ()
prompt nwords = do
    putStr "Enter a word> "
    hFlush stdout
    eof <- isEOF
    if eof
      then putStrLn ""
      else do
          getLine >>= putStrLn . correct nwords
          prompt nwords

main :: IO ()
main =
    readFile "big.txt" >>= return 
                         . train 
                         . lowerWords >>= prompt
