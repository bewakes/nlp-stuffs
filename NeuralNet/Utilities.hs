module Utilities(
    sigmoidMat
    , sigmoidMat'
)
where

import Data.Vector as V
import Matrix

sigmoid :: Float -> Float
sigmoid z = 1 / (1 Prelude.+ exp (-z))

sigmoidMat :: Matrix Float -> Matrix Float
sigmoidMat (Matrix m) = Matrix $ calcSigmoid $ m
    where calcSigmoid m = V.map sigElem m
          sigElem r = V.map sigmoid r

sigmoid' :: Float -> Float
sigmoid' z = sigmoid z Prelude.* ( 1 Prelude.- sigmoid z)

sigmoidMat' :: Matrix Float -> Matrix Float
sigmoidMat' (Matrix m) = Matrix $ V.map sigElem m
        where sigElem r = V.map sigmoid' r

reverseV :: [a] -> [a]
reverseV [] = []
reverseV (x:xs) = reverseV xs Prelude.++ [x]
