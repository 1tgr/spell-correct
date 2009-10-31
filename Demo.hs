module Tim.Spell.Correct.Demo where

import Data.Map
import IO
import Tim.Spell.Correct

prompt :: Map String Int -> IO ()
prompt nwords = do
    eof <- isEOF
    if eof
      then return ()
      else do
          getLine >>= putStrLn . correct nwords
          prompt nwords

main :: IO ()
main = readNWORDS >>= prompt
