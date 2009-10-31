module Tim.Spell.Tests where

import qualified Data.Map as Map
import HUnit
import List
import Tim.Spell.Correct

tests1 :: [ (String, String) ]
tests1 = [ ("access", "acess"), 
           ("accommodation", "accomodation acommodation acomodation") ]

tests2 :: [ (String, String) ]
tests2 = [ ("forbidden", "forbiden"), 
           ("decisions", "deciscions descisions"),
           ("supposedly", "supposidly") ]

doTest :: Map.Map String Int -> [ (String, String) ] -> Test
doTest nwords corrections = actualCorrections ~?= expectedCorrections
    where actualCorrections = nub
                            $ concatMap (map (correct nwords) . words . snd)
                            $ corrections
          expectedCorrections = map fst corrections

main :: IO Counts
main = do
    nwords <- readNWORDS
    runTestTT $ TestList [ "tests1 should pass" ~: doTest nwords tests1,
                           "tests2 should pass" ~: doTest nwords tests2 ]
