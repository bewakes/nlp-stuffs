import qualified Network as  N
import Matrix

{-net lst = do-}
    {-nn <- network lst-}
    {-putStrLn nn-}
    {-putStrLn nn-}
    {-return nn-}

training_data :: [(Matrix Float, Matrix Float)]
training_data = map create_tuple  ([
          [1,1,1]
        , [0.3, 0.2, 0]
        , [0.5,0.6, 0]
        , [0.6,0.7,1]
        , [0.8,0.6,1]
        , [0.0,0.6,0]
        , [0.5,0.9,0]
        , [0.8,0.9,1]
        , [0, 0.1, 0]
    ]::[[Float]])
    where create_tuple l = (matrixFromList [(take 2 l)], matrixFromList [(drop 2 l)])

eta = 0.3
mbsz = 2
epochs = 400
testdata = Nothing

main = do
    nn <- N.network [2,3]
    putStrLn $ show $ N.feedforward nn (matrixFromList [[0.0,0.9]])
    putStrLn $ show nn
    updatednn <- N.stochasticGD nn training_data epochs mbsz eta testdata
    putStrLn $show updatednn
    putStrLn $ show $ N.feedforward updatednn (matrixFromList [[0.0,0.9]])
