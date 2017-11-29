module Main where

import Control.Monad (replicateM)
import Control.Monad.Loops (iterateUntil)
import Data.List (intercalate)
import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import Text.Regex.Posix
import qualified Data.Text as T
import Text.Read (readMaybe)
import Text.Blaze
import Text.Blaze.Renderer.Pretty
import qualified Data.Text.IO as Tio

import TextGen (
  TextGen
  , Vocab
  , runTextGen
  , word
  , aan
  , choose
  , choose1
  , remove
  , weighted
  , list
  , randrep
  , perhaps
  , smartjoin
  , dumbjoin
  , upcase
  , postgen
  , loadVocab
  , tgempty
  )

type TextGenCh = TextGen StdGen [[Char]]

getDir (x:xs) = x
getDir _      = "./"

default_max_length :: Int
default_max_length = 280

maxLength :: [ String ] -> Int
maxLength (a:b:cs) = case readMaybe b of
  (Just i) -> i
  Nothing  -> default_max_length
maxLength _        = default_max_length

c :: Vocab -> String -> TextGenCh
c v s = choose $ v s

-- p50 = perhaps (1, 2)
-- p33 = perhaps (1, 3)
-- p66 = perhaps (2, 4)

al :: [ TextGenCh ] -> TextGenCh
al = aan . list

the :: TextGenCh -> TextGenCh
the g = list [ word "the", g ]


field :: Vocab -> TextGenCh
field v = choose [ f1, f2, f3 ]
  where f1 = region v (c v "colour")
        f2 = do
          ( c1, c2 ) <- choose $ v "colour"
          list [ region v c1, choose [ pattern v c2, border v c2 ] ]
        f3 = do
          ( c1, c2, c3 ) <- choose $ v "colour"
          list [ region v c1, pattern v c2, word "and", border v c3 ]

region :: Vocab -> TextGenCh -> TextGenCh
region v colour = list [ colour, c v "region" ]

pattern :: Vocab -> TextGenCh -> TextGenCh
pattern v colour = list [ c v "patterned", colour, c v "pattern" ]

border :: Vocab -> TextGenCh -> TextGenCh
border v colour = list [ c v "bordered", colour, c v "border" ]

-- this returns a complete description of an object (the first gen)
-- and a second gen which randomly makes shorter versions
--
-- ( "A blue cube decorated with red stripes",
--   choose [ "The blue cube", "The cube", "the blue cube with red stripes" ]

data Object = Object TextGenCh Bool

object :: Vocab -> TextGen StdGen ( TextGenCh, TextGenCh )
object v = do
  shape <- choose $ v "shape"
  pat <- choose $ v "pattern"
  ( shapec, patc ) <- choose $ v "colour"
  patwith <- return $ list [ c v "patterned", patc, pat ]
  primary <- return $ aan $ list [ shapec, shape, patwith  ]
  others <- return $ weighted [
    ( 25, word "it" ),
    ( 30, the shape ),
    ( 20, the $ list [ shapec, shape ] ),
    ( 10, the $ list [ shapec, shape, patwith ] )
    ]
  return ( primary, others )

-- TODO: substructures growing from or on other structures
-- plural structures
-- bigger vocabularies



-- appear: returns an appearance sentence, a primary description and
-- a secondary description, and a new list of objects

appear :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
appear v os = do
  ( primary, secondary ) <- object v
  bg <- return $ perhaps ( 1, 2 ) $ list [ c v "preposition", aan $ field v ]
  appearance <- return $ sentence [ primary, c v "appearance", bg ]
  os' <- return (os ++ [ secondary ])
  return ( appearance, os' )


-- disappear - removes one object from the population, returns a sentence
-- describing how it left, and retuns a new list of objects

disappear :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ])
disappear v os = do
  ( mo, os' ) <- remove os
  d <- return $ case mo of
                (Just o) -> sentence [ word $ dumbjoin o, c v "disappearance" ]
                Nothing -> sentence [ word "nothing happened" ]
  return ( d, os' )



-- extend creates a new object (or plural object) from an existing object

extend :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
extend v os = do
  o <- choose os
  ( primary, secondary ) <- object v
  os' <- return (os ++ [ secondary ] )
  return ( extension v o primary, os' )


-- plurals

extension :: Vocab -> TextGenCh -> TextGenCh -> TextGenCh
extension v o no = sentence [ word "From", o, c v "extends", no ]

-- interactions
act :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
act v os = case os of
  []        -> nullevent v os
  (a:[])    -> return ( act_intrans v os, os )
  otherwise -> do
    desc <- return $ choose [ act_trans v os, act_intrans v os ]
    return ( desc, os  )


act_trans :: Vocab -> [ TextGenCh ] -> TextGenCh
act_trans v os = do
  ( o1, o2 ) <- choose os
  sentence [ o1, c v "verbtrans", o2 ]

-- todo - active/passive

act_intrans :: Vocab -> [ TextGenCh ] -> TextGenCh
act_intrans v os = do
  o <- choose os
  sentence [ o, c v "verbintrans" ]


nullevent :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ])
nullevent v os = return ( word "----\n", os  )

-- take a list of objects, make a random event from the above. Return
-- a sentence describing the event, a new list of objects, and a boolean
-- flag indicating whether to paragraph break

event :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
event v os = case os of
               [] -> appear v os   -- chapter opening
               _  -> choose1 (nullevent v os) [
                 appear v os
                 , disappear v os
                 , act v os
                 , extend v os
                 ]



-- recursively generate sentences

event_r :: Vocab -> TextGenCh -> [ TextGenCh ] -> Int -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
event_r v body os count = do
  ( sent, os' ) <- event v os
  pbreak <- return $ choose [ word "\n\n", tgempty ]
  body' <- return $ list [ body, sent, pbreak ]
  case count - 1 < 0 of
    True -> return ( body', os' )
    False -> event_r v body' os' (count - 1) 
      






-- takes a list of generators, calls list on them and then
-- postgens sentence formatting

sentence :: [ TextGenCh ] -> TextGenCh
sentence g = postgen (\ws -> [ upcase $ (smartjoin ws ++ " ") ]) $ list g

-- take the output of a generator and add a newline
                     
para :: TextGenCh -> TextGenCh
para g = postgen (++ [ "\n\n" ]) g








formation :: Vocab -> TextGenCh
formation v = do
  ( text, os ) <- event_r v tgempty [] 2000
  text





main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  lines <- replicateM 1 $ do
    output <- getStdRandom $ runTextGen $ formation v
    return output
  putStrLn $ concat $ concat lines


