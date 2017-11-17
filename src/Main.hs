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

formation :: Vocab -> TextGenCh
formation v = do
  ( c1, c2 ) <- choose $ v "colour"
  let fg = aan $ list [ c1, c v "shape2d" ]
      bg = aan $ list [ c2, c v "region" ]
    in list [ fg, c v "juxtaposed", bg ]



main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  lines <- replicateM 100 $ do
    output <- getStdRandom $ runTextGen $ formation v
    return $ upcase $ smartjoin output
  putStrLn $ intercalate "\n" lines


