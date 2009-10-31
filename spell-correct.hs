module Main where

import Char  
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord
import IO
import List

lowerWords :: String -> [ String ]
lowerWords = filter (not . null)
           . map (map toLower . filter isAlpha) 
           . words

train :: [ String ] -> Map.Map String Int
train = List.foldl' (\dict word -> Map.insertWith' (+) word 1 dict) Map.empty 

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

edits1 :: String -> Set.Set String
edits1 word =
    let s = [ (take i word, drop i word) | i <- [ 0 .. length word ] ]
        deletes    = [ a ++ (tail b) | (a, b) <- s, not $ null b ]
        transposes = [ a ++ ((head $ tail b) : head b : drop 2 b) | (a, b) <- s, length b > 1 ]
        replaces   = [ a ++ (c : tail b) | (a, b) <- s, c <- alphabet, not $ null b ]
        inserts    = [ a ++ (c : b) | (a, b) <- s, c <- alphabet ]
    in Set.fromList (deletes ++ transposes ++ replaces ++ inserts)

known_edits2 :: Map.Map String Int -> String -> Set.Set String
known_edits2 nwords word = Set.fromList [ e2 | e1 <- Set.elems $ edits1 word, 
                                               e2 <- Set.elems $ edits1 e1, 
                                               Map.member e1 nwords ]

known :: Map.Map String Int -> Set.Set String -> Set.Set String
known nwords = Set.intersection (Map.keysSet nwords)

correct :: Map.Map String Int -> String -> String
correct nwords word = maximumBy (comparing (\c -> Map.findWithDefault undefined c nwords))
                    $ Set.elems
                    $ head
                    $ filter (not . Set.null)
                    $ [ known nwords $ Set.singleton word,
                        known nwords $ edits1 word,
                        known_edits2 nwords word,
                        Set.singleton word ]

prompt :: Map.Map String Int -> IO ()
prompt nwords = do
    eof <- isEOF
    if eof
      then return ()
      else do
          getLine >>= putStrLn . correct nwords
          prompt nwords

main :: IO ()
main =
    readFile "big.txt" >>= return 
                         . train 
                         . lowerWords >>= prompt
