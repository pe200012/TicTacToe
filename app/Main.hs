{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                  ( join )
import           Data.Array                     ( (!)
                                                , (//)
                                                , Array
                                                , Ix
                                                , assocs
                                                , bounds
                                                , indices
                                                , listArray
                                                )
import           Data.List                      ( dropWhileEnd )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , listToMaybe
                                                )
import           Data.Text.Format               ( format )
import qualified Data.Text.Lazy.IO             as T
import           System.IO
import           Text.Read                      ( readMaybe )

data Symbol = O | X deriving (Show, Eq)
newtype Board = Board { unBoard :: Array (Int, Int) (Maybe Symbol) } deriving Show

board :: Int -> Board
board len = Board (listArray ((0, 0), (len - 1, len - 1)) (repeat Nothing))

prettyprintBoard :: Board -> String
prettyprintBoard (Board b) = foldr
    (\((r, c), s) a ->
        (if r /= 0 && c == 0 then vBorder else "")
            ++ (case s of
                   Nothing -> ' '
                   Just s  -> head (show s)
               )
            :  '|'
            :  a
    )
    ""
    (assocs b)
  where
    len     = 1 + snd (snd (bounds b))
    vBorder = '\n' : take (2 * len - 1) (concat (repeat "-█")) ++ "\n"

displayBoard :: Board -> IO ()
displayBoard = putStrLn . prettyprintBoard

draw :: (Int, Int) -> Symbol -> Board -> Board
draw i s (Board b) | i `notElem` indices b = Board b
                   | isJust (b ! i)        = Board b
                   | otherwise             = Board (b // [(i, Just s)])

draws :: [((Int, Int), Symbol)] -> Board -> Board
draws = flip (foldr (uncurry draw))

check :: Board -> Maybe Symbol
check (Board b) = join $ listToMaybe $ concat
    (filter (\x -> length x >= 3 && allTheSame x && isJust (head x)) (fmap (b !) <$> ([column, row, dig1, dig2] <*> indices b)))
  where
    translate (r, c) = (c, upperBound - r)
    column (_, c0) = [ (r, c0) | r <- [0 .. upperBound] ]
    row (r0, _) = [ (r0, c) | c <- [0 .. upperBound] ]
    dig1 (r0, c0) = [ (r, c) | r <- [0 .. upperBound], c <- [0 .. upperBound], slash (translate (r0, c0)) (translate (r, c)) ]
    dig2 (r0, c0) = [ (r, c) | r <- [0 .. upperBound], c <- [0 .. upperBound], backslash (translate (r0, c0)) (translate (r, c)) ]
    allTheSame :: Eq a => [a] -> Bool
    allTheSame = all . ((==) . head) <*> tail
    slash (x0, y0) (x, y) = y == (-x) + b where b = x0 + y0
    backslash (x0, y0) (x, y) = y == x + b where b = y0 - x0
    upperBound = fst (snd (bounds b))

main :: IO ()
main = do
    c <- getHand
    go c (board 3)
  where
    go s g = do
        displayBoard g
        hFlush stdout
        case check g of
            Nothing -> do
                p <- getPos
                go (next s) (draw p s g)
            Just s' -> T.putStrLn (format "{} wins!" [show s'])
    next O = X
    next X = O
    getPos :: IO (Int, Int)
    getPos  = maybe getPos return . readMaybe =<< getLine
    getHand = do
        putStr "Which one first(O/X)?"
        hFlush stdout
        answer <- getLine
        case dropWhile (== ' ') $ dropWhileEnd (== ' ') answer of
            "O" -> return O
            "X" -> return X
            _   -> getHand
