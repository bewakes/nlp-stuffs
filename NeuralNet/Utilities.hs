module Utilities(
    sigmoidMat
    , sigmoidMat'
)
where

import Prelude as P
import Data.Vector as V
import Matrix
import System.Random

sigmoid :: Float -> Float
sigmoid z = 1 / (1 P.+ exp (-z))

sigmoidMat :: Matrix Float -> Matrix Float
sigmoidMat (Matrix m) = Matrix $ calcSigmoid $ m
    where calcSigmoid m = V.map sigElem m
          sigElem r = V.map sigmoid r

sigmoid' :: Float -> Float
sigmoid' z = sigmoid z P.* ( 1 P.- sigmoid z)

sigmoidMat' :: Matrix Float -> Matrix Float
sigmoidMat' (Matrix m) = Matrix $ V.map sigElem m
        where sigElem r = V.map sigmoid' r

reverseV :: [a] -> [a]
reverseV [] = []
reverseV (x:xs) = reverseV xs P.++ [x]


-- | rand function takes the upper limit
rand :: Int -> IO Int
rand n
    | n <= 0 = fmap (\x->0) (randomIO :: IO Float)
    | otherwise = fmap (truncate. (float_n P.*)) (randomIO :: IO Float)
    where float_n = fromIntegral n / 1.0

-- | shuffle function, which shuffles a list
shuffle :: [a] -> IO [a]
shuffle [] = fmap (\_ -> []) (randomIO :: IO Float)
shuffle lst = fmap (\y -> swap lst 0 y) (rand (P.length lst))

-- swap list elementsfmap (\(x, y) -> swap lst x y)
swap :: [a] -> Int -> Int -> [a]
swap lst a b 
    | a == b = lst
    | otherwise = (P.take a lst)
                    P.++ [lst P.!! b]
                    P.++ (P.take (b P.- a P.- 1) (P.drop (a P.+ 1) lst))
                    P.++ [lst P.!! a]
                    P.++ (P.drop (b P.+ 1) lst)
