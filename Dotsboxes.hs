module Dotsboxes where 

import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Console.GetOpt
import System.IO
import System.Exit
import Text.Read
import System.Environment
import Data.Char
--import Helper 


--          Data Types             --

-- Vertical lines always store the Top dot, horizontal store the left dot
data  Line = Horizontal  Dot | Vertical Dot deriving (Show, Eq, Ord, Read)
data Name = Player1 | Player2 deriving (Show, Eq, Read) 
data Outcome = Winner Name | Tie deriving (Show, Eq, Read) 
data Flag = Help | Win | Depth String | Move String | Verbose | Interactive deriving (Show, Eq)

type Dot = (Int, Int)
type BoardSize = (Int, Int)
type Gamestate = (BoardSize, [Line], [Box], [Box], Name)
type Box = Dot
type Rating = Int 


-- --For Full Credit-- 
-- readFlagInt str =
--   case readMaybe str of
--     Nothing -> do putStrLn "Invalid number. Exiting program."
--                   exitFailure
--     Just x -> return $ Just x
--     if not $ isGameOver game
--         then runGame $ updateGamestate game (readLine "") --we need to pass stuff form the command line into readLine
--         else putWinner game                 


lineToString :: Line -> String 
lineToString (Vertical (x,y)) = "Vertical (" ++ show (x+1) ++ "," ++ show (y+1) ++ ")"
lineToString (Horizontal (x,y)) = "Horizontal (" ++ show (x+1) ++ "," ++ show (y+1) ++ ")"

maybeLnToStr :: Maybe Line -> String
maybeLnToStr Nothing = "The gwame is ovwuh!"
maybeLnToStr (Just line) = lineToString line

-------------------- GAME LOGIC -------------------- 
isValidLine :: Gamestate -> Line -> Bool
isValidLine game line = 
    let size = getSize game 
        lines = getLines game

    in (isInBounds size line ) && (isAvailableLine lines line)

isInBounds :: BoardSize -> Line -> Bool
isInBounds (rows, columns) (Vertical (x,y)) = 
    (x >= 0) && (x <= (rows -1) ) && (y >= 0) && (y < (columns -1))
isInBounds (rows, columns) (Horizontal (x,y)) = 
    (x >= 0) && (x < (rows -1)) && (y >= 0) && (y <= (columns -1))
    --in not ((x1 < rows) && (x2 < rows) && (y1 < columns) && (y2 < columns) && (x1 > 0) && (x2 > 0) && (y1 > 0) && (y2 > 0))


isAvailableLine :: [Line] -> Line -> Bool 
isAvailableLine lines line = not(line `elem` lines)


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


 
--          Necessary Functions       --

--  parseLine necessary but will finish when we learn input/output

makeGamestate :: Int -> Int -> Gamestate
makeGamestate boardX boardY = ((boardX, boardY), [], [], [], Player1)



updateGamestate :: Gamestate -> Line -> Gamestate
updateGamestate (size, lines, p1Boxes, p2Boxes, Player1) move = 
    let newLines = if isValidLine (size, lines, p1Boxes, p2Boxes, Player1) move 
                        then move:lines
                        else error "Invalid Line"
        extraBoxes = makeBoxes lines move 
        nextPlayer = if null extraBoxes then Player2 else Player1
    in  (size, newLines, extraBoxes++p1Boxes, p2Boxes, nextPlayer)

updateGamestate (size, lines, p1Boxes, p2Boxes, Player2) move = 
    let newLines = if isValidLine (size, lines, p1Boxes, p2Boxes, Player2) move 
                        then move:lines
                        else error "Invalid Line"
        extraBoxes = makeBoxes lines move 
        nextPlayer = if null extraBoxes then Player1 else Player2
    in  (size, newLines, p1Boxes, extraBoxes++p2Boxes, nextPlayer)


--          Display function and helperfunctions          --
displayGameState:: Gamestate -> String
displayGameState game = 
    let totalY = (snd (getSize game)) - 1
    in concat [displayRow y game | y <- [0..totalY]]

displayRow :: Int -> Gamestate -> String
displayRow currentY ((x,_), lines, boxes1, boxes2, _) = (displayRowHelper1 x 0 currentY lines) ++ (displayRowHelper2 x 0 currentY lines boxes1 boxes2)


displayRowHelper1 :: Int -> Int -> Int->  [Line] -> String
displayRowHelper1 totalX currentX currentY lines = 
    let horizontal = displayHorizontal (currentX, currentY) lines
        helper = displayRowHelper1 totalX (currentX + 1) currentY lines 
    in if totalX == (currentX)
            then "\n"
        else horizontal ++ helper

displayRowHelper2 :: Int -> Int -> Int->  [Line] -> [Box] -> [Box] -> String
displayRowHelper2 totalX currentX currentY lines boxes1 boxes2 = 
    let vertical = displayVertical (currentX, currentY) lines boxes1 boxes2
        helper = displayRowHelper2 totalX (currentX + 1) currentY lines boxes1 boxes2
    in if totalX == (currentX)
        then "\n"
    else vertical ++ helper

displayHorizontal :: Dot -> [Line] -> String
displayHorizontal _ [] = "o   "
displayHorizontal dot  ((Horizontal lineDot):lines) 
    | lineDot == dot =  "o\x2500\x2500\x2500" --"o---"
    | otherwise = displayHorizontal dot lines
displayHorizontal dot  ((Vertical _):lines) = 
    displayHorizontal dot lines


displayVertical :: Dot -> [Line] -> [Box]-> [Box] -> String
displayVertical _ [] _ _ = "    "
displayVertical dot ((Vertical lineDot):lines) boxes1 boxes2 
    | lineDot /= dot = displayVertical dot lines boxes1 boxes2
    | dot `elem` boxes1 = "\x2502 1 "
    | dot `elem` boxes2 = "\x2502 2 "
    | otherwise = "\x2502   "
displayVertical dot ((Horizontal _):lines) boxes1 boxes2 = 
    displayVertical dot lines boxes1 boxes2 

isVertical:: Line -> Bool 
isVertical (Vertical _) = True 
isVertical (Horizontal _) = False  

--replace empty string with player input line


putWinner :: Gamestate -> IO ()
putWinner game = putStrLn $ winnerString (findWinner game)

winnerString :: Maybe Outcome -> String
winnerString (Just (Winner name)) = "The winner is: " ++ show name
winnerString (Just Tie) = "Game tied."
winnerString Nothing = "Game ongoing."

findWinner :: Gamestate -> Maybe Outcome
findWinner (size, lines, boxes1, boxes2, turn)
    | finished && len1 > len2 = Just (Winner Player1)
    | finished && len1 < len2 = Just (Winner Player2)
    | finished && len1 == len2 = Just Tie
    | otherwise = Nothing  
    where len1 = length boxes1
          len2 = length boxes2
          finished = isGameOver (size, lines, boxes1, boxes2, turn)

isGameOver :: Gamestate -> Bool
isGameOver ((x,y), _, boxes1, boxes2, _ ) =
    let totalBoxes = (x-1) * (y -1)
        madeBoxes = boxes1 ++ boxes2
    in length madeBoxes == totalBoxes

bestMove :: Gamestate -> Maybe Line
bestMove game = 
    let possibleLines = makePossibleLines game
    in case findWins game possibleLines of 
            Nothing -> case findTies game possibleLines of
                            Nothing -> Just (head possibleLines)
                            Just line -> (Just line)
            Just line -> (Just line)

findWins :: Gamestate -> [Line] -> Maybe Line
findWinds _ [] = Nothing
findWins game (l:ls) =
    if whoWillWin (updateGamestate game l) == Winner (getName game)
        then Just l 
        else findWins game ls

findTies :: Gamestate -> [Line] -> Maybe Line
findTies _ [] = Nothing
findTies game (l:ls) = 
    if whoWillWin (updateGamestate game l) == Tie
        then Just l 
        else findTies game ls

whoWillWin :: Gamestate -> Outcome
whoWillWin game  = 
    let possibleLines =  makePossibleLines game
        winner = findWinner game
    in case winner of 
        (Just outcome) -> outcome
        _ -> bestOutcome [whoWillWin (updateGamestate game line) | line <- possibleLines] (getName game)

bestOutcome :: [Outcome] -> Name -> Outcome 
bestOutcome outcomeLst player
    | Winner player `elem` outcomeLst = Winner player
    | Tie `elem` outcomeLst = Tie
    | otherwise = Winner $ otherPlayer player 

evaluateBoard :: Gamestate -> Int 
evaluateBoard game = 
    let p1Score = length (getBoxes1 game)
        p2Score = length (getBoxes2 game)
        (x,y) = getSize game
        --current = getName
    in case (findWinner game) of
        Just (Winner Player1) -> x*y
        Just (Winner Player2) -> (-x)*y
        Just Tie -> 0
        --Add a point if it is your turn, possibly?? 
        Nothing -> p1Score - p2Score

--FINISH THIS--
whoMightWin :: Gamestate -> Int -> Rating 
whoMightWin game 0 = evaluateBoard game
whoMightWin game num = 
    let possibleLines  =  makePossibleLines game
        winner = findWinner game
    in case winner of
        (Just _) -> evaluateBoard game
        _ -> bestRating [whoMightWin (updateGamestate game line) (num-1)| line <- possibleLines] (getName game)
--Fix^^^
bestRating :: Ord a => [a] -> Name -> a 
bestRating ratingLst Player1 = maximum ratingLst
bestRating ratingLst Player2 = minimum ratingLst

possibleBestMove :: Gamestate -> Int -> Line
possibleBestMove game num =
    snd (bestRating ratings name)
    where possibleLines =  makePossibleLines game
          ratings = [(whoMightWin (updateGamestate game line) num, line) | line <- possibleLines]
          name = getName game

     
--           IO Functions           --
--type Gamestate = (BoardSize, [Line], [Box], [Box], Name)

-- readGame2 :: String -> Maybe Gamestate --Is this ok?
-- readGame2 string = read string :: Maybe Gamestate

showGame2 :: Gamestate -> String
showGame2 game = show game 

readGame :: String -> Maybe Gamestate
readGame "" = Nothing
readGame string = 
    let split = splitOn "\n" string
        (size:lines:boxes1:boxes2:player:[]) = split
    in Just (readSize size, readLineLst lines, readBoxes boxes1, readBoxes boxes2,readName player)

readSize :: String -> BoardSize
readSize str = (head intStr, last intStr)
    where intStr = map read (splitOn " " str) :: [Int]


readLineLst :: String -> [Line]
readLineLst "" = []
readLineLst string =  
    let lines = splitOn "," string
    in map readLine lines 


readLine :: String -> Line 
readLine string =
    case line of 
        ("H":x:y:[]) -> Horizontal ((read x::Int),(read y::Int))
        ("V":x:y:[]) -> Vertical ((read x::Int),(read y::Int))
        _ -> error "Not valid line"
    where line = splitOn " " string
                        
readBoxes :: String -> [Box]
readBoxes "" = []
readBoxes str = 
    let tuples = splitOn "," str
        stringboxes = map (splitOn " ") tuples
        boxes = map (\s -> ((read (head s)::Int),(read (last s)::Int))) stringboxes
    in boxes

readName :: String -> Name
readName str 
   | str == "P1" = Player1
   | otherwise = Player2

loadGameIO :: String -> IO (Maybe Gamestate)
loadGameIO "" = return Nothing
loadGameIO string = 
    do contents <- (readFile string)
       return (readGame contents)

showGame :: Gamestate -> String
showGame (size, lines, boxes1, boxes2, turn) = (showSize size) ++ "\n" ++ (showLines lines) ++ "\n" ++ (showBoxes boxes1) ++ "\n" ++ (showBoxes boxes2) ++ "\n" ++ (showName turn)

showSize :: BoardSize -> String
showSize (x,y) = (show x) ++ " " ++ (show y)

showLines :: [Line] -> String
showLines lines = intercalate "," (map showLineHelper lines)

showLineHelper :: Line -> String
showLineHelper (Vertical (x,y)) = "V " ++ (show x) ++ " " ++ (show y)
showLineHelper (Horizontal (x,y)) = "H " ++ (show x) ++ " " ++ (show y) 

showBoxes :: [Box] -> String
showBoxes boxes = intercalate "," (map showBoxHelper boxes) 

showBoxHelper :: Box -> String
showBoxHelper (x,y) = (show x) ++ " " ++ (show y)

showName :: Name -> String
showName Player1 = "P1"
showName Player2 = "P2"

writeGame :: Gamestate -> String -> IO ()
writeGame game fileName = writeFile fileName gameString
    where gameString = showGame game



-- makeAllLines :: BoardSize -> [Int] -> [Line]
-- makeAllLines size [i] = (makeHorizontalLines size)++(makeVerticalLines size)
-- makeAllLines size (i:is) = (makeHorizontalLines size)++(makeVerticalLines size)++(makeAllLines size is)

makePossibleLines :: Gamestate -> [Line]
makePossibleLines ((x,y), lines, _, _, _) = 
   let allLines = makeAllLines ((x-1),(y-1)) 0
   in filter (\x -> not (x `elem` lines)) allLines

-- on first pass should not make horizontal lines extra argument is either 0 on first pass or 1 on
-- all others
makeAllLines :: BoardSize -> Int -> [Line]
makeAllLines (0,y) _ = (makeHorizontalLines (0,y)) ++ (makeVerticalLines (0,y))
makeAllLines (x,y) 0 = (makeVerticalLines (x,y)) ++ (makeAllLines ((x-1),y) 1)
makeAllLines (x,y) _ = (makeHorizontalLines (x,y)) ++ (makeVerticalLines (x,y)) ++ (makeAllLines ((x-1),y) 1)


-- for a 3x3 you need to pass (0,2) (1,2) (2,2)
makeHorizontalLines :: (Int,Int) -> [Line]
makeHorizontalLines (x,y) = [Horizontal (x,y1) | y1 <- [0..y]]
makeVerticalLines (x,y) = [Vertical (x,y1) | y1 <- [0..y-1]]
--isValidMove :: BoardSize -> Line -> Bool
--isValidMove size line = line `elem` (makeAllLines size)
--makeMove = undefined --(does this take two dots or a line?? bc if line then no need and if two dots already written?)


--           Maybe helpful stuff           -- 

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (\x -> x `elem` ys) xs

otherPlayer :: Name -> Name 
otherPlayer current 
    | current == Player1 = Player2
    | otherwise = Player1 

getSize :: Gamestate -> BoardSize
getSize (size, _, _, _, _) = size

getName :: Gamestate -> Name
getName (_, _, _, _, name) = name

getBoxes1 :: Gamestate -> [Box]
getBoxes1 (_,_,boxes,_,_) = boxes

getBoxes2 :: Gamestate -> [Box]
getBoxes2 (_,_,_,boxes,_) = boxes

getLines :: Gamestate -> [Line]
getLines (_,lines,_,_,_) = lines


checkLineDot :: Line -> Dot -> Bool 
checkLineDot line dot = (getDot line) == dot

getDot:: Line -> Dot 
getDot (Vertical dot) = dot 
getDot (Horizontal dot) = dot

first :: (a,b,c) -> a 
first (x,_,_) = x

second :: (a,b,c) -> b 
second (_,x,_) = x

third :: (a,b,c) -> c 
third (_,_,x) = x

isInt x = x == fromInteger (round x)
