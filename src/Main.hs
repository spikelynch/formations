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

sentence :: [ [ Char ] ] -> String
sentence = upcase . smartjoin

-- for the TextGen library:
-- take a [ TextGenCh ] list some of which will be empty,
-- gen from them, remove null lists, and then perform some
-- intercalation on them.

-- perhaps / depends

-- depends :: Maybe TextGenCh -> TextGenCh
-- depends (Just g) = g
-- depends Nothing  = tgempty


-- Formations

-- field

-- field :: Vocab -> [ TextGenCh ] -> TextGenCh
-- field v (c1:c2:c3:[]) = al [ basefield v c1, pattern v c2, border v c3 ] ]
-- field v (a:b:[])   = al [ basefield v c1, choose [ pattern v c2, border v c2 ] ]
-- field v (a:[])     = aan $ basefield v c1
-- field _ _          = tgempty



field :: Vocab -> TextGenCh
field v = choose [ f1, f2, f3 ]
  where f1 = aan $ region v (c v "colour")
        f2 = do
          ( c1, c2 ) <- choose $ v "colour"
          aan $ list [ region v c1, choose [ pattern v c2, border v c2 ] ]
        f3 = do
          ( c1, c2, c3 ) <- choose $ v "colour"
          aan $ list [ region v c1, pattern v c2, word "and", border v c3 ]

region :: Vocab -> TextGenCh -> TextGenCh
region v colour = list [ colour, c v "region" ]

pattern :: Vocab -> TextGenCh -> TextGenCh
pattern v colour = list [ c v "patterned", colour, c v "pattern" ]

border :: Vocab -> TextGenCh -> TextGenCh
border v colour = list [ c v "bordered", colour, c v "border" ]

object1 v = do
  shape <- choose $ v "shape"
  basec <- choose $ v "colour"
  short <- return $ list [ word "a", basec, shape ]
  long <- return $ list [ word "a complicated", shape, word "of the colour", basec ]
  ( short, long )

-- this returns a complete description of an object (the first gen)
-- and a second gen which randomly makes shorter versions
--
-- ( "A blue cube decorated with red stripes",
--   choose [ "The blue cube", "The cube", "the blue cube with red stripes" ]

-- object :: Vocab -> TextGen StdGen ( TextGenCh, TextGenCh )
-- object v = do
--   shape <- choose $ v "shape"
--   ( mainc, secondc ) <- choose $ v "colour"
--   p <- return $ pattern v secondc
--   ( aan $ list [ mainc, shape, p ], list [ word "the", shape ] )
  -- primary <- return $ aan $ list [ mainc, shape, p ]
  -- others <- return $ weighted [
  --   ( 10, list [ word "the", mainc, shape, p ] ),
  --   ( 30, list [ word "the", shape ] ),
  --   ( 20, list [ word "the", mainc, shape ] )
  --   ]
  -- ( primary, others )




formation :: Vocab -> TextGenCh
formation v = do
  ( short, long ) <- object1 v
  -- ( complete, abbrev ) <- object v
  list [ short, long ]




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
    return $ sentence output
  putStrLn $ intercalate "\n" lines


