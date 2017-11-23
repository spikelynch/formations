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
  , weighted
  , list
  , randrep
  , perhaps
  , smartjoin
  , dumbjoin
  , upcase
  , postgen
  , loadVocab
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


-- for the TextGen library:
-- take a [ TextGenCh ] list some of which will be empty,
-- gen from them, remove null lists, and then perform some
-- intercalation on them.

-- perhaps / depends

-- depends :: Maybe TextGenCh -> TextGenCh
-- depends (Just g) = g
-- depends Nothing  = tgempty





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

object :: Vocab -> TextGen StdGen ( TextGenCh, TextGenCh )
object v = do
  shape <- choose $ v "shape2d"
  pat <- choose $ v "pattern"
  ( shapec, patc ) <- choose $ v "colour"
  patwith <- return $ list [ c v "patterned", patc, pat ]
  primary <- return $ aan $ list [ shapec, shape, patwith  ]
  others <- return $ weighted [
    ( 30, shape ),
    ( 20, list [ shapec, shape ] ),
    ( 10, list [ shapec, shape, patwith ] )
    ]
  return ( primary, others )


-- things which objects can do
-- intransitive
--     appear
--     disappear
--     move
--     transform into other objects
--     multiply

-- transitive (to other objects, or fields)
--     consume
--     merge with
--     disappear into
--     obliterate or hide
--     conceal themselves within

-- an event is
-- one or several objects, and a field
-- a series of acts between random combos of the objects
--
-- each act is a function from the objects to the new objects
-- not removing them for now

-- need a combinator to generate a random number of things


the g = list [ word "the", g ]


objects :: Vocab -> TextGen StdGen [ ( TextGenCh, TextGenCh ) ]
objects v = do
  o1 <- object v
  o2 <- object v
  o3 <- object v
  return [ o1, o2, o3 ] 

act :: Vocab -> [ ( TextGenCh, TextGenCh ) ] -> TextGenCh  
act v os = choose [ act_trans v ps, act_intrans v ps ]
  where ps = map ( \( _, p ) -> p ) os

act_trans :: Vocab -> [ TextGenCh ] -> TextGenCh
act_trans v os = do
  ( o1, o2 ) <- choose os
  v <- choose $ v "verbtrans"
  list [ the o1, v, the o2 ]

-- todo - active/passive

act_intrans :: Vocab -> [ TextGenCh ] -> TextGenCh
act_intrans v os = do
  o <- choose os
  v <- choose $ v "verbintrans"
  list [ the o, v ]


formation :: Vocab -> TextGenCh
formation v = do
  os <- objects v
  a <- return $ sentence $ act v os
  list [ a, a, a, a, a ]

-- take the output of a generator and sentence-format it

sentence :: TextGenCh -> TextGenCh
sentence g = postgen (\ws -> [ upcase $ smartjoin ws ]) g

-- take the output of a generator and add a newline
                     
para :: TextGenCh -> TextGenCh
para g = postgen (++ [ "\n" ]) g







--formation :: Vocab -> TextGenCh
--formation v = field v

  -- list [ fg, c v "juxtaposed", bg ]
  -- where fg = aan $ list [ c v "colour", c v "shape2d" ]
  --       bg = field v


--


main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  lines <- replicateM 10 $ do
    output <- getStdRandom $ runTextGen $ formation v
    return output
  putStrLn $ intercalate "\n" $ concat lines


