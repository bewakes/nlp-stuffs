module Helpers(
    afterNthElement
    , removeTrailingNonChar
    , freqList
    , binarySearchAs
    , binarySearch
    , binaryGetCountAs
    , binaryGetCount
    , strLower
)
where

import Data.Map
import Data.Char
import qualified Data.Vector as V

-----------------------------------------------------
-- String manipulation
-- --------------------------------------------------
strLower :: String -> String
strLower [] = []
strLower (x:xs) = (toLower x) : (strLower xs)

-----------------------------------------------------
-- Function for extracting elements after nth element
-----------------------------------------------------
afterNthElement :: Int -> ([a] -> [a])
afterNthElement 0 = id
afterNthElement n = tail . (afterNthElement (n-1))

------------------------------------------------
-- Function for removing trailing non characters
------------------------------------------------
removeTrailingNonChar :: String -> String
removeTrailingNonChar x = (reverse . remove . reverse . remove) x
    where remove [] = []
          remove (x:xs)
            | x `elem` chars = (x:xs)
            | otherwise = remove xs
          chars = ['a'..'z'] ++['A'..'Z']

countElem :: (Ord k) => Data.Map.Map k Int -> k -> Data.Map.Map k Int
countElem m e = Data.Map.insertWith (\n o -> n + o) e 1 m

freqList :: (Ord k) => [k] -> Map k Int
freqList = Prelude.foldl countElem empty


--------------------------
-- BINARY SEARCH --
--------------------------
-- This takes equality function to compare
binarySearchAs :: Ord a => (a -> a -> Ordering) -> a -> V.Vector a ->Int -> Int -> Int
binarySearchAs cfunc elem list l u
    | V.null list == True  = -1
    | u < l = -1
    | otherwise = case elem `cfunc` midElem of
        EQ -> midIndex
        LT -> binarySearchAs cfunc elem list l (midIndex-1)
        GT -> binarySearchAs cfunc elem list (midIndex+1) u
    where midIndex = (l + u) `div` 2
          midElem = list V.! midIndex

binarySearch :: Ord a => a -> V.Vector a -> Int -> Int -> Int
binarySearch e ls l h = binarySearchAs compare e ls l h

-- TODO: fix bug
binaryGetCountAs :: Ord a => (a -> a -> Ordering) -> a -> V.Vector a -> Int -> Int -> Int
binaryGetCountAs cfunc elem list l u
    | indx == -1 = 0
    | otherwise = right - left + 1

    where indx = binarySearchAs cfunc elem list l u
          left = leftMargin elem list 0 indx
          right = rightMargin elem list indx u
          leftMargin e lst l u -- u has the element
            | u-l==1 && elem `cfunc` (lst V.! l) == EQ = l
            | u-l==1 && elem `cfunc` (lst V.! l) == LT = u
            | u==l  = l
            | otherwise = let midElem = lst V.! midIndex
                          in case e `cfunc` midElem of
                            EQ -> leftMargin e lst l (midIndex)
                            GT -> leftMargin e lst (midIndex+1) u
                            LT -> 0 -- this is error, right now don't know what to do
            where midIndex = (l+u) `div` 2

          rightMargin e lst l u -- l has the element
            | u-l==1 && elem `cfunc` (lst V.! u) == EQ = u
            | u-l==1 && elem `cfunc` (lst V.! u) == GT = l
            | u==l  = u
            | otherwise = let midElem = lst V.! midIndex
                          in case e `cfunc` midElem of
                            EQ -> rightMargin e lst (midIndex) u
                            LT -> rightMargin e lst l (midIndex-1)
                            GT -> 0 -- this is error, right now don't know what to do
            where midIndex = (l+u) `div` 2

binaryGetCount :: Ord a => a -> V.Vector a -> Int -> Int -> Int
binaryGetCount elem list l u = binaryGetCountAs compare elem list l u
