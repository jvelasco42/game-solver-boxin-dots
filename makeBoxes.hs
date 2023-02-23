
import Data.Ratio ((%))
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace



--          Data Types             --
--
--


-- Vertical lines always store the Top dot, horizontal store the left dot
data  Line = Horizontal  Dot | Vertical Dot deriving (Show, Eq) 
--data Player = Player [Box] Name deriving Show
--type Player = ([Box], Name)
type Dot = (Int, Int)
type BoardSize = (Int, Int)--potentially a 2d list of Dots
type Gamestate = (BoardSize, [Line], [Box], [Box], Name)
type Box = Dot
data Name = Player1 | Player2

-- makeBoxes
makeBoxes :: [Line] -> Line -> [Box]
makeBoxes lines (Horizontal (x,y)) = 
    let dotUp = (x, y-1) 
        dotUpRight = (x+1, y-1) 
        dotRight = (x+1, y)
        dotDown = (x, y+1)
        upperBox = [Horizontal dotUp, Vertical dotUp, Vertical dotUpRight]
        lowerBox = [Horizontal dotDown, Vertical (x,y), Vertical dotRight]
        checkAbove = if upperBox `subset` lines then [dotUp] else []
        checkBelow = if lowerBox `subset` lines then [(x,y)] else []
    in checkAbove++checkBelow++[]

makeBoxes lines (Vertical (x,y)) = 
    let dotLeft = (x-1, y)
        dotDownLeft = (x-1, y+1) 
        dotRight = (x+1, y)
        dotDown = (x, y+1)
        leftBox = [Horizontal dotLeft, Vertical dotLeft, Horizontal dotDownLeft]
        rightBox = [Horizontal (x,y), Vertical dotRight, Horizontal dotDown]
        checkLeft = if leftBox `subset` lines then [dotLeft] else []
        checkRight = if rightBox `subset` lines then [(x,y)] else []
    in checkLeft++checkRight++[]

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (\x -> x `elem` ys) xs
