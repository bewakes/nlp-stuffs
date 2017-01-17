module Helpers(
    afterNthElement
    , removeTrailingNonChar
    , freqList
    , binarySearchAs
    , binarySearch
    , binaryGetCountAs
    , binaryGetCount
)
where

import Data.Map

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
binarySearchAs :: Ord a => (a -> a -> Ordering) -> a -> [a] ->Int -> Int -> Int
binarySearchAs cfunc elem list l u
    | list == [] = -1
    | u < l = -1
    | otherwise = case elem `cfunc` midElem of
        EQ -> midIndex
        LT -> binarySearchAs cfunc elem list l (midIndex-1)
        GT -> binarySearchAs cfunc elem list (midIndex+1) u
    where midIndex = (l + u) `div` 2
          midElem = list !! midIndex

binarySearch :: Ord a => a -> [a] -> Int -> Int -> Int
binarySearch e ls l h = binarySearchAs compare e ls l h

-- TODO: fix bug
binaryGetCountAs :: Ord a => (a -> a -> Ordering) -> a -> [a] -> Int -> Int -> Int
binaryGetCountAs cfunc elem list l u
    | indx == -1 = 0
    | otherwise = right - left + 1

    where indx = binarySearchAs cfunc elem list l u
          left = leftMargin elem list 0 indx
          right = rightMargin elem list indx u
          leftMargin e lst l u -- u has the element
            | u-l==1 && lst!!l ==elem  = l
            | u-l==1 && lst!!l /= elem = u
            | otherwise = let midElem = lst !! midIndex
                          in case midElem `cfunc` e of
                            EQ -> leftMargin e lst l (midIndex-1)
                            LT -> leftMargin e lst (midIndex+1) u
                            GT -> 0 -- this is error, right now don't know what to do
            where midIndex = (l+u) `div` 2

          rightMargin e lst l u -- l has the element
            | u-l==1 && lst!!u ==elem = u
            | u-l==1 && lst!!u /= elem = l
            | otherwise = let midElem = lst !! midIndex
                          in case midElem `cfunc` e of
                            EQ -> rightMargin e lst (midIndex+1) u
                            GT -> rightMargin e lst l (midIndex-1)
                            LT -> 0 -- this is error, right now don't know what to do
            where midIndex = (l+u) `div` 2

binaryGetCount :: Ord a => a -> [a] -> Int -> Int -> Int
binaryGetCount elem list l u = binaryGetCountAs compare elem list l u
