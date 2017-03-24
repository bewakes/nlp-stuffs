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
