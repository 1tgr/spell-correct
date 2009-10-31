module Tim.Spell.Correct where

import Char  
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord
import List

lowerWords = filter (not . null) . map (map toLower . filter isAlpha) . words

train = List.foldl' (\dict word -> Map.insertWith' (+) word (1::Int) dict) Map.empty 

readNWORDS = readFile "big.txt" >>= return . train . lowerWords

alphabet = [ 'a' .. 'z' ]

edits1 word =
    let s = [ (take i word, drop i word) | i <- [ 0 .. length word ] ]
        deletes    = [ a ++ tail b | (a, b) <- s, not $ null b ]
        transposes = [ a ++ (b!!1 : b!!0 : drop 2 b) | (a, b) <- s, not $ null b, not $ null $ tail b ]
        replaces   = [ a ++ (c : tail b) | (a, b) <- s, c <- alphabet, not $ null b ]
        inserts    = [ a ++ (c : b) | (a, b) <- s, c <- alphabet ]
    in Set.fromList (deletes ++ transposes ++ replaces ++ inserts)

known_edits2 knownWords = Set.unions . Set.elems . Set.map (Set.intersection knownWords . edits1) . edits1

correct nwords word = 
    let knownWords = Map.keysSet nwords
        candidates = Set.elems
                   $ head
                   $ filter (not . Set.null)
                   $ [ Set.intersection knownWords $ Set.singleton word,
                       Set.intersection knownWords $ edits1 word,
                       known_edits2 knownWords word,
                       Set.singleton word ]
      in maximumBy (comparing (\w -> w `Map.lookup` nwords)) candidates
