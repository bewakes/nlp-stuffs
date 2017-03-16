module Matrix ()
where

import qualified Data.Vector as V
import qualified Data.List as L

data Matrix a = V.Vector (V.Vector a)
