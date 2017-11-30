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

type TextGenBool = TextGen StdGen Bool

getDir (x:xs) = x
getDir _      = "./"


default_length :: Int
default_length = 1000

getLength :: [ String ] -> Int
getLength (a:b:cs) = case readMaybe b of
  (Just i) -> i
  Nothing  -> default_length
getLength _        = default_length

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

tgbtrue :: TextGenBool
tgbtrue = return True

tgbfalse :: TextGenBool
tgbfalse = return False


object :: Vocab -> TextGen StdGen ( TextGenCh, TextGenCh, Bool )
object v = do
  plural <- choose1 tgbfalse [ tgbtrue, tgbfalse ] 
  shape <- choose $ v ( if plural then "shapes" else "shape" )
  pat <- choose $ v "pattern"
  ( shapec, patc ) <- choose $ v "colour"
  patwith <- return $ list [ c v "patterned", patc, pat ]
  primary <- return $ case plural of
    True -> list [ c v "some", shapec, shape, patwith ]
    False -> list [ aan shapec, shape, patwith ] 
  secondary <- return $ weighted [
    ( 25, word (if plural then "they" else "it") ),
    ( 40, the shape ),
    ( 50, the $ list [ shapec, shape ] )
    ]
  return ( primary, secondary, plural )




-- TODO: substructures growing from or on other structures
-- plural structures
-- bigger vocabularies



-- appear: returns an appearance sentence, a primary description and
-- a secondary description, and a new list of objects

appear :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
appear v os = do
  ( primary, secondary, plural ) <- object v
  cht <- return $ case os of
    [] -> chtitle secondary
    otherwise -> perhaps ( 1, 10 ) $ chtitle secondary
  bg <- return $ perhaps ( 1, 2 ) $ list [ c v "preposition", aan $ field v ]
  appearance <- return $ list [ cht, sentence [ primary, binflect plural (c v "appearance"), bg ] ]
  os' <- return (os ++ [ secondary ])
  return ( appearance, os' )


chapter :: Vocab -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
chapter v = do
  ( appearance, os ) <- appear v []
  o <- return $ head os
  return ( chtitle o, os ) 


-- disappear - removes one object from the population, returns a sentence
-- describing how it left, and retuns a new list of objects

disappear :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ])
disappear v os = do
  ( mo, os' ) <- remove os
  d <- return $ case mo of
                (Just o) -> sentence [ inflect (word $ dumbjoin o) (c v "disappearance") ]
                Nothing -> sentence [ word "nothing happened" ]
  return ( pp 30 d, os' )



-- extend creates a new object (or plural object) from an existing object

extend :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
extend v os = do
  o <- choose os
  ( primary, secondary, plural ) <- object v
  os' <- return (os ++ [ secondary ] )
  return ( pp 10 (extension v o primary plural), os' )




extension :: Vocab -> TextGenCh -> TextGenCh -> Bool -> TextGenCh 
extension v o no pl = choose [ e1, e2 ]
  where e1 = sentence [ word "From", accusative o, binflect pl (c v "growth"), no ]
        e2 = sentence [ inflect o (c v "produce"), no ]


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
  pp 15 $ sentence [ inflect o1 (c v "action_trans"), accusative o2 ]

accusative :: TextGenCh -> TextGenCh
accusative g = postgen (\w -> if w == [ "they" ] then [ "them" ]  else w) g


-- todo - active/passive

act_intrans :: Vocab -> [ TextGenCh ] -> TextGenCh
act_intrans v os = do
  o <- choose os
  pp 15 $ sentence [ inflect o (c v "action_intrans") ]


-- dumb-ass inflection: if the noun doesn't end in s, append s to the
-- verb

inflect :: TextGenCh -> TextGenCh -> TextGenCh
inflect noun verb = postgen inflect_l $ list [ noun, verb ]
  where inflect_l ws = case ispluralphrase ws of
          True -> ws
          False -> pluralverb ws


ispluralphrase :: [[ Char ]] -> Bool
ispluralphrase ("they":ws) = True
ispluralphrase ws          = case secondlast ws of
                               Nothing  -> False
                               (Just w) -> ispluralnoun w
          
secondlast :: [ [ Char ] ] -> Maybe [ Char ]
secondlast ws = secondfirst $ reverse ws
  where secondfirst (x:y:ys) = Just y
        secondfirst _        = Nothing
          
ispluralnoun :: [ Char ] -> Bool
ispluralnoun w = isplural $ reverse w
  where isplural (x:xs) = (x == 's')
        isplural _      = False

-- For where the sentence structure doesn't fit inflect

binflect :: Bool -> TextGenCh -> TextGenCh
binflect False verb = postgen (\ws -> pluralverb ws) verb
binflect True  verb = verb


pluralverb :: [[ Char ]] -> [[ Char ]]
pluralverb ws = case es $ reverse ws of
                  True -> appendp ws "es"
                  False -> appendp ws "s"
  where es [] = False
        es (w:ws) = case reverse w of
                      ('s':cs) -> True
                      ('h':'c':cs) -> True
                      ('h':'s':cs) -> True
                      ('z':cs) -> True
                      ('o':cs) -> True
                      otherwise -> False
            
appendp :: [[ Char ]] -> [ Char] -> [[ Char ]]
appendp ws s = reverse $ appendp_ $ reverse ws
  where appendp_ (w:ws) = (w ++ s):ws
        appendp_ []     = []

nullevent :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ])
nullevent v os = return ( word "----\n", os  )

-- take a list of objects, make a random event from the above. Return
-- a sentence describing the event, a new list of objects, and a boolean
-- flag indicating whether to paragraph break

event :: Vocab -> [ TextGenCh ] -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
event v os = case os of
               [] -> chapter v  
               _  -> choose1 (nullevent v os) [
                 appear v os
                 , disappear v os
                 , disappear v os
                 , act v os
                 , act v os
                 , extend v os
                 , extend v os
                 ]



-- recursively generate sentences

event_r :: Vocab -> TextGenCh -> [ TextGenCh ] -> Int -> TextGen StdGen ( TextGenCh, [ TextGenCh ] )
event_r v body os count = do
  ( sent, os' ) <- event v os
  body' <- return $ list [ body, sent ]
  case count - 1 < 0 of
    True -> return ( body', os' )
    False -> event_r v body' os' (count - 1) 
      






-- takes a list of generators, calls list on them and then
-- postgens sentence formatting

sentence :: [ TextGenCh ] -> TextGenCh
sentence g = postgen (\ws -> [ upcase $ (smartjoin ws ++ " ") ]) $ list g

-- chapter title
-- This disappears if the title is "It" or "They"

chtitle :: TextGenCh -> TextGenCh
chtitle g = postgen cht g
  where cht ("it":ws)   = []
        cht ("they":ws) = []
        cht ws          =  [ "\n\n## " ++ (upcase $ dumbjoin ws) ++ "\n\n"]

        
-- take the output of a generator and add a newline
                     
para :: TextGenCh -> TextGenCh
para g = postgen (++ [ "\n\n" ]) g

pp :: Int -> TextGenCh -> TextGenCh
pp p g = weighted [ ( p, para g), (100 - p, g) ]




formations :: Vocab -> Int -> TextGenCh
formations v c = do
  ( text, os ) <- event_r v tgempty [] c
  text


sshape = choose $ map word [ "they", "triangle","rhombuses", "it", "they" ]

test_inflect :: Vocab -> TextGenCh
test_inflect v = list [ s1, s1, s1, s1, s1, s1, s1, s1, s1, s1 ]
  where s1 = sentence [ inflect sshape (c v "action_intrans") ]


main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  l <- return $ getLength args
  lines <- replicateM 1 $ do
    output <- getStdRandom $ runTextGen $ formations v l
    return output
  putStrLn $ concat $ concat lines


