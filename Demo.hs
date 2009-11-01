module Tim.Spell.Demo where

import IO
import Tim.Spell.Correct

main :: IO ()
main = do
    nwords <- readNWORDS
    interact $ unlines . map (correct nwords) . lines
