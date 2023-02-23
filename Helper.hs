module Helper where 

import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Console.GetOpt
import System.IO
import System.Exit
import Text.Read
import System.Environment ()
import Data.Char
import Dotsboxes 


-- --           Maybe helpful stuff           -- 

-- subset :: Eq a => [a] -> [a] -> Bool
-- subset xs ys = all (\x -> x `elem` ys) xs

-- otherPlayer :: Name -> Name 
-- otherPlayer current 
--     | current == Player1 = Player2
--     | otherwise = Player1 

-- getSize :: Gamestate -> BoardSize
-- getSize (size, _, _, _, _) = size

-- getName :: Gamestate -> Name
-- getName (_, _, _, _, name) = name

-- getBoxes1 :: Gamestate -> [Box]
-- getBoxes1 (_,_,boxes,_,_) = boxes

-- getBoxes2 :: Gamestate -> [Box]
-- getBoxes2 (_,_,_,boxes,_) = boxes

-- getLines :: Gamestate -> [Line]
-- getLines (_,lines,_,_,_) = lines


-- checkLineDot :: Line -> Dot -> Bool 
-- checkLineDot line dot = (getDot line) == dot

-- getDot:: Line -> Dot 
-- getDot (Vertical dot) = dot 
-- getDot (Horizontal dot) = dot

-- first :: (a,b,c) -> a 
-- first (x,_,_) = x

-- second :: (a,b,c) -> b 
-- second (_,x,_) = x

-- third :: (a,b,c) -> c 
-- third (_,_,x) = x