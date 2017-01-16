module Ngrams(
    bigrams, 
    old_bigrams,
    old_trigrams, 
    trigrams,
    ngrams,
    old_ngrams,
    m_ngrams,
    m_bigrams,
    skipBigrams
) where

import Helpers
import Data.List -- for 'tails' function
import Control.Monad -- for 'guard' function
------------------------------------------------
-- primitive Function for bigrams
------------------------------------------------
old_bigrams :: [a] -> [[a]]
old_bigrams (x:y:xs) = [[x, y]]++ old_bigrams (y:xs)
old_bigrams _ = []

------------------------------------------------
-- primitive Function for trigrams
------------------------------------------------
old_trigrams :: [a] -> [[a]]
old_trigrams (x:y:z:xs) = [[x,y,z]] ++ old_trigrams (y:z:xs)
old_trigrams _ = []

------------------------------------------------
-- primitive Function for ngrams
------------------------------------------------
old_ngrams :: Int -> [a] -> [[a]]
old_ngrams n l
    | n <= 0 = []
    | length l < n = []
    | otherwise = [take n l] ++ old_ngrams n (tail l)

------------------------------------------------
-- Function for generating skipping bigrams
------------------------------------------------
old_skipBigrams :: [a] -> [[a]]
old_skipBigrams [] = []
old_skipBigrams (x:xs) = firstWithAll (x:xs) ++ old_skipBigrams xs
    where firstWithAll (x:y:xs) = [[x,y]] ++ firstWithAll (x:xs)
          firstWithAll _ = []

------------------------------------------------
-- elegant skipBigrams
------------------------------------------------
skipBigrams :: [a] -> [[a]]
skipBigrams = foldl (++) [] . map (\x-> [[a,b] | a<-[head x], b<-tail x]) . filter ((<=) 2 .length) . tails

------------------------------------------------
-- Sexy implementation of ngrams
------------------------------------------------
ngrams n = filter ((==) n . length ) . map (take n) . tails
bigrams = ngrams 2
trigrams = ngrams 3


------------------------------------------------
-- Monadic defination of ngrams
------------------------------------------------
m_ngrams n lst = do
    t <- tails lst
    ng <- [take n t]
    guard (length ng == n)
    return ng

m_bigrams  = m_ngrams 2
