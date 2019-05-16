import Data.Char
import Data.List
import System.IO
import Control.Monad

-- Board dimensions
size :: Int
size = 3

-- Clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- Move cursor to the given coordinates
goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

-- Produces the next Player
next :: Player -> Player
next O = X
next B = B
next X = O

-- Creates an empty grid with size `size`
empty :: Grid
empty = replicate size (replicate size B)

-- Checks if grid has been filled completely
full :: Grid -> Bool
full = all (/= B) . join

-- Determines whose turn it is
turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = join g

-- Determines if a given player has won
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..(size-1)]]

-- Determines if any player has won on the given Grid
won :: Grid -> Bool
won g = wins O g || wins X g

-- Prints a grid to the screen
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . join . interleave bar . map showRow
  where
    bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- Determines if a given index is a valid move
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && join g !! i == B

-- Performs a move and produces a singleton list containing the new Grid if valid,
--   otherwise returns the empty list
move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,_:ys) = splitAt i $ join g

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = l : chop n r
  where
    (l,r) = splitAt n xs

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      putStrLn "ERROR: Invalid number" >>= (const $ getNat prompt)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- AI: Game Tree

data Tree a = Node a [Tree a]
            deriving Show

instance Functor Tree where
  fmap f (Node x xs) = Node (f x) $ (fmap . fmap) f xs

instance Foldable Tree where
  foldMap f (Node x xs) = (f x) <> (mconcat (fmap (foldMap f) xs))

treesize :: Tree a -> Int
treesize (Node _ xs) = foldl (\a c -> a + treesize c) 1 xs

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = join [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g []) | wins O g  = Node (g,O) []
                    | wins X g  = Node (g,X) []
                    | otherwise = Node (g,B) []
minimax (Node g ts) = Node (g, rs) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_,p) _ <- ts']
    rs = case turn g of
           O -> minimum ps
           X -> maximum ps

-- assumes list is nonempty
findextreme :: (a -> Int) -> (Int -> Int -> Bool) -> [a] -> a
findextreme f cmp (x:xs) =
  snd (foldl' (\a c -> if cmp (f c) (fst a) then (f c,c) else a) (f x,x) xs)

bestmove :: Int -> Grid -> Player -> Grid
bestmove depth g p = bg
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree
    possible = [Node (g',p') v | Node (g',p') v <- ts, p' == best]
    Node (bg, _) _ = findextreme treesize (<) possible

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Choose a difficulty from 1-3:"
          depth <- promptDepth
          play depth empty O

promptDepth :: IO Int
promptDepth = getLine >>= \line ->
                case line of
                  "1" -> return 2
                  "2" -> return 5
                  "3" -> return 9
                  _   -> putStrLn "Invalid choice. Please choose a difficulty from 1-3:" >>= const promptDepth

play :: Int -> Grid -> Player -> IO ()
play depth g p = do cls
                    goto (1,1)
                    putGrid g
                    play' depth g p

play' :: Int -> Grid -> Player -> IO ()
play' depth g p | wins O g = putStrLn "Player O wins!\n"
                | wins X g = putStrLn "Player X wins!\n"
                | full g   = putStrLn "It's a draw!\n"
                | p == O   = getNat (prompt p) >>= \i ->
                               case move g i p of
                                 []   -> putStrLn "ERROR: Invalid move" >>= (const $ play' depth g p)
                                 [g'] -> play depth g' (next p)
                | p == X   = putStr "Player X is thinking... " >>= (const $ (play depth $! (bestmove depth g p)) (next p))
