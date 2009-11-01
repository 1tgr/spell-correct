module Tim.Spell.Correct where

import Char  
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord
import List

lowerWords = filter (not . null) 
           . map (map toLower . filter isAlpha) 
           . words

train w = Map.fromListWith (+) 
        $ zip w 
        $ repeat (1::Int)

readNWORDS = fmap (train . lowerWords) 
           $ readFile "big.txt"

alphabet = [ 'a' .. 'z' ]

edits1 word = let s = zip (inits word) (tails word)
                  deletes    = [ a ++ y     | (a, _:y  ) <- s ]
                  transposes = [ a ++ y:x:z | (a, x:y:z) <- s ]
                  replaces   = [ a ++ c:y   | (a, _:y  ) <- s, c <- alphabet ]
                  inserts    = [ a ++ c:x   | (a, x    ) <- s, c <- alphabet ]
               in Set.fromList $ concat [ deletes, transposes, replaces, inserts ]

known_edits2 knownWords = Set.unions 
                        . Set.elems 
                        . Set.map (Set.intersection knownWords . edits1) 
                        . edits1

correct nwords word = let knownWords = Map.keysSet nwords
                          candidates = Set.elems
                                     $ head
                                     $ filter (not . Set.null)
                                     $ [ Set.intersection knownWords $ Set.singleton word,
                                         Set.intersection knownWords $ edits1 word,
                                        known_edits2 knownWords word,
                                        Set.singleton word ]
                       in maximumBy (comparing (`Map.lookup` nwords)) candidates
