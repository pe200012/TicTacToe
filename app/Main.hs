{-# LANGUAGE ViewPatterns #-}
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
import           Data.Maybe                     ( isJust
                                                , listToMaybe
                                                )

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

draw :: (Int, Int) -> Symbol -> Board -> Board
draw i s (Board b) | i `notElem` indices b = Board b
                   | isJust (b ! i)        = Board b
                   | otherwise             = Board (b // [(i, Just s)])

draws :: [((Int, Int), Symbol)] -> Board -> Board
draws = flip (foldr (uncurry draw))

check :: Board -> Maybe Symbol
check (Board b) = join $ listToMaybe $ concat (filter (\x -> length x >= 3 && allTheSame x && isJust (head x)) (fmap (b !) <$> ([column, row, dig1, dig2] <*> indices b)))
  where
    translate (r, c) = (c, upperBound - r)
    column (_, c0) = [(r, c0) | r <- [0..upperBound]]
    row    (r0, _) = [(r0, c) | c <- [0..upperBound]]
    dig1   (r0, c0) = [(r, c) | r <- [0..upperBound], c <- [0..upperBound], slash (translate (r0, c0)) (translate (r, c)) ]
    dig2   (r0, c0) = [(r, c) | r <- [0..upperBound], c <- [0..upperBound], backslash (translate (r0, c0)) (translate (r, c)) ]
    allTheSame :: Eq a => [a] -> Bool
    allTheSame = all . ((==) . head) <*> tail
    slash (x0, y0) (x, y) = y == (-x) + b where b = x0 + y0
    backslash (x0, y0) (x, y) = y == x + b where b = y0 - x0
    upperBound = fst (snd (bounds b))

main :: IO ()
main = pure ()
