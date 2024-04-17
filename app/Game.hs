{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TupleSections #-}

-- Make a game (or any simple interactive app) by replacing the
-- GameState data type below and the three functions initGameState,
-- gamePrompt, and gameStep with your own.  You should also create
-- whatever additional data types and functions you need.

module Game where
import Text.Read (readMaybe)
import Data.Set as S (Set, fromList, member, insert, empty)
import Data.List (uncons)

-- | A type to keep track of the current state of the game. Change
--   this to whatever type makes sense for your game.  You will most
--   likely want to define other types as well.
data GameState where

  -- Prompt for size of grid.
  -- random numbers
  SizeP      :: [Int] -> GameState
  -- Prompt for number of mines.
  -- Grid size, random numbers
  MinesP     :: Int -> [Int] -> GameState
  -- Choose action.
  -- Number of mines, grid size, grid
  Sweeping   :: Int -> Int -> [[Cell]] -> [Int] -> GameState
  -- Select column.
  -- Action, n mines, grid size, grid
  SelectingC :: Action -> Int -> Int -> [[Cell]] -> [Int] -> GameState
  -- Select row.
  -- Action, location, n mines, grid size, grid
  SelectingR :: Action  -> Int -> Int-> Int -> [[Cell]] -> [Int] -> GameState
  Lost       :: [Int] -> GameState
  Won        :: [Int] -> GameState

data Action where
  Sweep :: Action
  Flag  :: Action

data Cell where
  -- Is a mine
  Flagged :: Bool -> Cell
  -- Is a mine, number of bordering mines
  Open    :: Bool -> Int -> Cell
  -- Is a mine
  Closed  :: Bool -> Cell
  deriving Show

-- | The initial 'GameState' value that should be used when the game
--   starts.  Takes an infinite list of randomly generated integers in
--   the range (0, 10^6) as input, which you are free to use or ignore
--   as you wish.
initGameState :: [Int] -> GameState

-- EXAMPLE, replace the below with your own implementation.
initGameState = SizeP

-- | Given the current 'GameState', decide what prompt to show when
--   asking the user to enter input.
gamePrompt :: GameState -> String

-- EXAMPLE, replace the below with your own implementation.
gamePrompt (SizeP _)  = "How large would you like the grid? "
gamePrompt (MinesP _ _) = "How many mines would you like to sweep for? "
gamePrompt (Sweeping _ _ g _) = drawGrid drawCell g ++ "\nWould you like to flag a mine or open a cell? (f/o) "
gamePrompt (SelectingC {}) = "What column would you like? "
gamePrompt (SelectingR {}) = "Waht row would you like? "
gamePrompt _               = "Play again? (y/n) "

-- | The main workhorse of your game: given a 'String' representing
--   the input entered by the player at a prompt, and the current
--   'GameState', return (1) a message to be printed, and (2) possibly
--   a new/updated 'GameState', or 'Nothing' if the game is over.  Of
--   course you can and should decompose this into many smaller
--   functions.
gameStep :: String -> GameState -> (String, Maybe GameState)

gameStep size gs@(SizeP r) = case readMaybe size of
  Just i -> ("Creating "++show i++" by "++show i++" grid.", Just $ MinesP i r)
  Nothing -> ("Invalid input, please input a number.", Just gs)
gameStep mines gs@(MinesP i r) = case readMaybe mines of
  Just m -> ("Creating "++show m++" mines.", Just $ Sweeping m i grid is)
    where
      (is, grid) = createGrid r m i
  Nothing -> ("Invalid input, please input a number,", Just gs)
gameStep fo gs@(Sweeping m s g r)
  | fo `elem` ["f", "F"] = ("Placing a flag.", Just $ SelectingC Flag m s g r)
  | fo `elem` ["o", "O"] = ("Opening a cell.", Just $ SelectingC Sweep m s g r)
  | otherwise = ("Invalid input, please enter f or o.", Just gs)
gameStep col gs@(SelectingC a m s g r) = case readMaybe col of
  Just c -> if c > 0 && c <= s
    then ("Selecting column "++show c++".", Just $ SelectingR a (c - 1) m s g r)
    else ("Column not in grid, please try again", Just gs)
  Nothing -> ("Invalid input, please input a number.", Just gs)
gameStep row gs@(SelectingR a c m s g rand) = case readMaybe row of
  Just r -> if r > 0 && r <= s
    then case a of
      Sweep -> clearCell (c, r - 1) m s rand g
      Flag  -> placeFlag (c, r - 1) m s rand g
    else ("Row not in grid, please try again", Just gs)
  Nothing -> ("Invalid input, please input a number.", Just gs)
gameStep yn (Won r)
  | yn `elem` ["y", "Y"] = ("Playing again!", Just $ SizeP r)
  | yn `elem` ["n", "N"] = ("Goodbye!", Nothing)
  | otherwise = ("Please enter either y or n.", Just $ Won r)
gameStep yn (Lost r)
  | yn `elem` ["y", "Y"] = ("Playing again!", Just $ SizeP r)
  | yn `elem` ["n", "N"] = ("Goodbye!", Nothing)
  | otherwise = ("Please enter either y or n.", Just $ Lost r)

-- Places a flag on the grid if the location given is a valid closed cell.
placeFlag :: (Int, Int) -> Int -> Int -> [Int] -> [[Cell]] -> (String, Maybe GameState)
placeFlag (c, r) m s rand g = case cell of
  (Flagged True) -> ("Removing flag from cell "++show c++", "++show r++".",
    Just $ Sweeping m s (replaceCell (c, r) (Closed True) g) rand)
  (Flagged False) -> ("Removing flag from cell "++show c++", "++show r++".",
    Just $ Sweeping m s (replaceCell (c, r) (Closed False) g) rand)
  (Open _ _) -> ("Cannot place a flag on an open cell", Just $ Sweeping m s g rand)
  (Closed True) -> ("Placing flag in cell "++show c++", "++show r++".",
      Just $ Sweeping m s (replaceCell (c, r) (Flagged True) g) rand)
  (Closed False) -> ("Placing flag in cell "++show c++", "++show r++".",
    Just $ Sweeping m s (replaceCell (c, r) (Flagged False) g) rand)
  where
    cell = g !! r !! c

-- Checks if a board is won.
isWonBoard :: [[Cell]] -> Bool
isWonBoard = all isWonRow

-- Checks if a row is won.
isWonRow :: [Cell] -> Bool
isWonRow [] = True
isWonRow (cell:r) = case cell of
  (Flagged False) -> False
  (Closed False)  -> False
  _               -> isWonRow r

-- Clears a cell if it is a valid closed cell.
clearCell :: (Int, Int) -> Int -> Int -> [Int] -> [[Cell]] -> (String, Maybe GameState)
clearCell (c, r) m s rand g = case cell of
  (Flagged _) -> ("Cannot clear a flagged cell.", Just $ Sweeping m s g rand)
  (Open _ _)  -> ("Cannot clear an open cell.", Just $ Sweeping m s g rand)
  (Closed True) -> ("Opps! That was a mine. You blew up!\n" ++ drawGrid drawRevealed g, Just $ Lost rand)
  (Closed False) -> if isWonBoard ng
    then ("You cleared the board! Congratulations!\n" ++ drawGrid drawRevealed ng, Just $ Won rand)
    else ("Opening new cells.", Just $ Sweeping m s ng rand)
  where
    ng = clearClosedCell [(c, r)] g S.empty
    cell = g !! r !! c

-- Clears a cell. Given cell is expected to be closed.
clearClosedCell :: [(Int, Int)] -> [[Cell]] -> S.Set (Int, Int) -> [[Cell]]
clearClosedCell [] g _ = g
clearClosedCell (l:ls) g v
  | S.member l v = clearClosedCell ls g v
  | m == 0 = clearClosedCell (ls++next) ng nv
  | otherwise = clearClosedCell ls ng nv
  where
    next = getCellNeighbors l checkNeighbor ng
    nv = S.insert l v
    ng = replaceCell l (Open False m) g
    m = getMineCount l g

-- Get number of mines surrounding a cell.
getMineCount :: (Int, Int) -> [[Cell]] -> Int
getMineCount l g = length $ getCellNeighbors l checkMines g

-- Checks if a cell is a mine.
checkMines :: (Int, Int) -> [[Cell]] -> (Int, Int) -> Bool
checkMines o g (c, r) = ((c, r) /= o) && (case g !? r of
    Nothing  -> False
    Just row -> case row !? c of
      Nothing   -> False
      Just cell -> case cell of
        Closed b  -> b
        Flagged b -> b
        _         -> False)

-- Gets the neighbors of a cell.
getCellNeighbors :: (Int, Int) -> ((Int, Int) -> [[Cell]] -> (Int, Int) -> Bool) -> [[Cell]] -> [(Int, Int)]
getCellNeighbors (c, r) f g = filter (f (c, r) g) $ cartesianProduct [c-1..c+1] [r-1..r+1]

-- Checks if a cell is a non-mine closed cell.
checkNeighbor :: (Int, Int) -> [[Cell]] -> (Int, Int) -> Bool
checkNeighbor o g (c, r) = ((c, r) /= o) && (case g !? r of
    Nothing  -> False
    Just row -> case row !? c of
      Nothing   -> False
      Just cell -> case cell of
        Closed b -> not b
        _        -> False)

-- !! but returns a maybe.
-- I thought this was in prelude or Data.List but it wouldn't work
(!?) :: [a] -> Int -> Maybe a
(!?) ls i = if i >= 0 && i < length ls then Just $ ls !! i else Nothing

-- Replaces a cell in the grid.
replaceCell :: (Int, Int) -> Cell -> [[Cell]] -> [[Cell]]
replaceCell (col, row) c cs = f ++ [insertCell' col c m] ++ bcs
  where
    (m, bcs) = case uncons b of
      Just (m', bcs') -> (m', bcs')
      Nothing -> error "index out of bounds for insert cell"
    (f, b) = splitAt row cs

-- Inserts a cell into a row.
insertCell' :: Int -> Cell -> [Cell] -> [Cell]
insertCell' i c cs = f ++ [c] ++ bcs
  where
    bcs = tail b
    (f, b) = splitAt i cs

-- Shuffle a list using a stream of random integers. Borrowed from
-- https://stackoverflow.com/questions/14692059/how-to-shuffle-a-list
shuffle :: [Int] -> [a] -> ([Int], [a])
shuffle [] xs = ([], xs)
shuffle r []  = (r, [])
shuffle (i:is) xs = (is', head rest : ls)
  where
    (firsts, rest) = splitAt (i `mod` length xs) xs
    (is', ls) = shuffle is (firsts ++ tail rest)

-- Gets random coordinates for the mines.
getRandomCoords :: [Int] -> Int -> Int -> ([Int], S.Set (Int, Int))
getRandomCoords r m s = (is, S.fromList $ take m sh)
  where
    (is, sh) = shuffle r $ cartesianProduct [0..s-1] [0..s-1]

-- Cartesian Product of two lists.
cartesianProduct :: Ord a => [a] -> [b] -> [(a, b)]
cartesianProduct s1 s2 = concatMap (\a -> map (a, ) s2) s1

-- Generates a list of row values from the mines.
genRowValues :: S.Set (Int, Int) -> Int -> Int -> [Cell]
genRowValues ms r i =  Closed (S.member (i, r) ms) : genRowValues ms r (i+1)

-- Creates a row.
createRow :: S.Set (Int, Int) -> Int -> Int -> [Cell]
createRow ms r s = take s $ genRowValues ms r 0

-- Generates a list of rows.
genRows :: S.Set (Int, Int) -> Int -> Int -> [[Cell]]
genRows ms s i = createRow ms i s : genRows ms s (i+1)

-- Creates the grid.
createGrid :: [Int] -> Int -> Int -> ([Int], [[Cell]])
createGrid rNums nMines size = (is, take size $ genRows ms size 0)
  where
    (is, ms) = getRandomCoords rNums nMines size

-- Draws a grid given a drawing function.
drawGrid :: (Cell -> Char) -> [[Cell]] -> String
drawGrid f =  unlines . map (map f)

-- Draws the grid with the mines hidden.
drawCell :: Cell -> Char
drawCell (Flagged _)    = 'F'
drawCell (Open True _)  = 'M'
drawCell (Open False 0) = ' '
drawCell (Open False i) = head $ show i
drawCell (Closed _)     = '#'

-- Draws the grid with the mines displayed.
drawRevealed :: Cell -> Char
drawRevealed (Flagged _)    = 'F'
drawRevealed (Open True _)  = 'M'
drawRevealed (Open False 0) = ' '
drawRevealed (Open False i) = head $ show i
drawRevealed (Closed False) = '.'
drawRevealed (Closed True)  = 'M'