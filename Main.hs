module Main where 

import Dotsboxes
import Helper
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

----------------------- MAIN ----------------------- 
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)               "Print usage information and exits."
          , Option ['w'] ["winner"] (NoArg Win)              "Print out the best move, using an exhaustive search (no cut-off depth)."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>")    "Use <num> as a cutoff depth, instead of your default."
          , Option ['m'] ["move"] (ReqArg Move "<move>")     "Make <move> and print out the resulting board, in the input format, to stdout.\ 
                                                             \\n The move should be 1-indexed. If a move requires multiple values, the move \
                                                             \will \nbe a tuple of numbers separated by a comma with no space."
          , Option ['v'] ["verbose"] (NoArg Verbose)         "Output both the move and a description of how good it is: win, lose, tie, or a rating."
          , Option ['i'] ["interactive"] (NoArg Interactive) "Start a new game and play against the computer. Make sure this is compatible with the -d flag."
          ]

main :: IO ()
main = 
    do args <- getArgs
       let (flags, inputs, errors) = getOpt Permute options args
       putStrLn $ show (flags, inputs, errors)-- comment out when done 
       let filename = case inputs of
                          [] -> "defaultGame.txt"
                          [fname] -> fname 
                          _ -> error "Too many inputs!"
    --    case flags of
    --         Help `elem` flags -> putStrLn $ usageInfo "Usage: dotsboxes [options] [file]" options
    --         Interactive `elem` flags -> putStrLn "running interactive game" --startInteractive 0
    --         _ ->  do contents <- readFile filename 
    --                 runMaybeGame flags (readGame contents)
       
       if Help `elem` flags
         then putStrLn $ usageInfo "Usage: dotsboxes [options] [file]" options
         else if Interactive `elem` flags
                then do depth <- getDepth flags 
                        case depth of 
                            Nothing -> do game <- startInteractive
                                          putStrLn $ instructionsMsg ++ (displayGameNewLine game)
                                          runInteractive game 2
                            Just x -> do game <- startInteractive
                                         putStrLn $ instructionsMsg ++ (displayGameNewLine game)
                                         runInteractive game x
                else do contents <- readFile filename 
                        runMaybeGame flags (readGame contents)

getDepth :: [Flag] -> IO (Maybe Int)
getDepth [] = return Nothing 
getDepth ((Depth x):_) = readFlagJustInt x 
getDepth (_:flags) = getDepth flags 

runInteractive :: Gamestate -> Int -> IO()
runInteractive game num = 
    if not $ isGameOver game
        then case getName game of 
             Player1 -> do line <- (getPlayerLine game "Enter a move in the format Horizontal (<num>, <num>) ")
                           let uGame = updateGamestate game (line)
                           putStrLn $ displayGameNewLine uGame
                           runInteractive (uGame) num
             Player2 -> let newGame = updateGamestate game (possibleBestMove game num)
                        in do putStrLn $ "Computer's Move : " ++ displayGameNewLine newGame
                              runInteractive newGame num
        else putWinner game

startInteractive = 
    -- let x = getNumber "\nEnter the number of columns you would like to use: "
    --     y = (getNumber "\nEnter the number of rows you would like to use: ")
    do x <- getNumber "\nEnter the number of columns you would like to use"
       y <- getNumber "\nEnter the number of rows you would like to use"
       return ((x, y), [], [], [], Player1)

runGame :: [Flag] -> Gamestate -> IO ()
runGame [] game = putStrLn $ possibleMoveMsg ++ lineToString (possibleBestMove game 2) ++ displayGameNewLine game
runGame (Win:flags) game = 
    let bm = bestMove game
    in case bm of 
        Nothing -> error "Not Valid Move!"
        Just move -> if checkVerbose flags 
                        then putStrLn $ runWin move game ++ runVerbose move game ++ displayGameNewLine game 
                        else putStrLn $ runWin move game ++ displayGameNewLine game
runGame ((Depth str):flags) game = 
    do num <- readFlagInt str
       let pbm = possibleBestMove game num
       --fix to run with interactive
       if checkVerbose flags 
           then putStrLn $ (runDepth pbm game ) ++ (runVerbose pbm game) ++ displayGameNewLine game 
           else putStrLn $ runDepth pbm game ++ displayGameNewLine game
runGame ((Move str):flags) game = 
    do line <- readFlagLine str
       let uGame = updateGamestate game line
       if checkVerbose flags 
           then  putStrLn $ displayGameNewLine game ++(runMove line) ++ (runVerbose line game) ++ displayGameNewLine uGame 
           else putStrLn $ (runMove line) ++  displayGameNewLine uGame 
-- runGame (Interactive:flags) game = undefined
--     putStrLn runInteractive(game 2)


runGame ([Verbose]) game = 
    let line = possibleBestMove game 2
        newGame = updateGamestate game line
    in putStrLn $ possibleMoveMsg ++ lineToString (line) ++ (runVerbose line game) ++ displayGameNewLine newGame
runGame (Verbose:flags) game = runGame (flags ++ [Verbose]) game

displayGameNewLine :: Gamestate -> String 
displayGameNewLine game = "\n" ++ displayGameState game

runMove :: Line -> String 
runMove line = moveMsg ++ lineToString line 
    
runVerbose :: Line ->  Gamestate -> String
runVerbose line game = 
    let newGame = updateGamestate game line 
        rating = evaluateBoard newGame
    in "\n" ++ verboseMsg ++ show rating
    --in "\n" ++ verboseMsg ++ show rating ++ displayGameNewLine game ++ "\n Not updated verbose: " ++ showGame game ++ "\n Updated Verbose: " ++ showGame newGame

runDepth :: Line -> Gamestate -> String
runDepth line game = possibleMoveMsg ++ lineToString (line)

runWin :: Line -> Gamestate -> String
runWin line game = bestMoveMsg ++ lineToString (line)

--Player1 is the person, Player2 is the computer 
--startInteractive :: String -> Gamestate


checkVerbose :: [Flag] -> Bool 
checkVerbose flags = Verbose `elem` flags 


 
-- (Horizontal,(0,0))

runMaybeGame :: [Flag] -> Maybe Gamestate -> IO ()
runMaybeGame _ Nothing = putStrLn "no game"
runMaybeGame flags (Just game) = runGame flags game

readFlagLine str =
    case strLine of 
        ("Horizontal":x:y:[]) -> do intX <- readFlagInt x
                                    intY <- readFlagInt y
                                    return (Horizontal (intX, intY))
        ("Vertical":x:y:[]) -> do intX <- readFlagInt x
                                  intY <- readFlagInt y
                                  return (Vertical (intX, intY))
        _ -> do putStrLn "Invalid Line. Exiting program."
                exitFailure
        where noParenthesis = filter (\i -> i /= '(' && i /= ')') str
              strLine = splitOn "," noParenthesis


readFlagInt str =
  case readMaybe str of
    Nothing -> do putStrLn "Invalid number. Exiting program."
                  exitFailure
    Just x -> return x

readFlagJustInt str =
  case readMaybe str of
    Nothing -> do putStrLn "Invalid number. Exiting program."
                  exitFailure
    Just x -> return $ Just x
--resetGame 
--generateEmptyGame 
possibleMoveMsg:: String 
possibleMoveMsg = "Possible Move: "

bestMoveMsg :: String
bestMoveMsg = "Your Ideal Move Is: "

moveMsg :: String
moveMsg = "Your Move Was: "

verboseMsg :: String 
verboseMsg = "The Rating of Line: " 

instructionsMsg :: String
instructionsMsg = "\nThe game is played by placing a vertical line or horizontal and attempt\ 
                   \ to make boxes. \nThe player that makes the most boxes will win. \nThe game\ 
                   \ will promt you to enter your move and you should input your line as the\ 
                   \ following examples: \nHorizontal (1,1)  \nVertical (2,3) \n"

promptSizeMsg :: String 
promptSizeMsg = "Enter the size of the board you wish to use in the format (<num>,<num>)"

prompt :: String -> IO String
prompt question =
  do putStr $ question ++ ": "
     hFlush stdout
     getLine

getNumber :: String -> IO Int
getNumber question =
  do answer <- prompt question
     case readMaybe answer of
        Just x -> if x <= 0 || x > 10
                    then do putStrLn "Not a good number, try again."
                            getNumber question
                    else return x
        Nothing -> do putStrLn "Not a good number, try again."
                      getNumber question
                      --need to check for Horizontal or Vertical Line
getPlayerLine :: Gamestate -> String -> IO Line
getPlayerLine game question = 
  do answer <- prompt question
     case readMaybe answer of
        Just (Vertical (x,y)) -> let line = Vertical (x - 1, y -1)
                                 in if isValidLine game line 
                                       then return line
                                       else do putStrLn "Not a valid line."
                                               getPlayerLine game "Try again."   
        Just (Horizontal (x,y)) -> let line = Horizontal (x - 1, y -1)
                                   in if isValidLine game line 
                                         then return line
                                         else do putStrLn "Not a valid line."
                                                 getPlayerLine game "Try again." 
        Nothing -> do putStrLn "Not a valid line."
                      getPlayerLine game "Try again."