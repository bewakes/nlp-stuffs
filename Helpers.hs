module Helpers(
    afterNthElement,
    removeTrailingNonChar,
    freqList
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
