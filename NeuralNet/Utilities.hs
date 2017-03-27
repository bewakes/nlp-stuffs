module Utilities(
)
where

import Data.Vector as V
import Matrix

sigmoid :: Float -> Float
sigmoid z = 1 / (1 + exp (-z))

sigmoidMat :: Matrix Float -> Matrix Float
sigmoidMat (Matrix m) = Matrix $ calcSigmoid $ m
    where calcSigmoid m = V.map sigElem m
          sigElem r = V.map sigmoid r

sigmoid' :: Float -> Float
sigmoid' z = sigmoid z Prelude.* ( 1 Prelude.- sigmoid z)

sigmoidMat' :: Matrix Float -> Matrix Float
sigmoidMat' (Matrix m) = Matrix $ V.map sigElem m
        where sigElem r = V.map sigmoid' r

