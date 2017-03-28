module Network (
    Network(..)
)
where

import Matrix
import Utilities


data Network = Network {
    layerSizes :: [Int],
    biases :: [Matrix Float],
    weights :: [Matrix Float],
    numLayers :: Int
}

-- | Network constructor,  takes in layerSizes, initializes biases and weights
-- |    returns Network
-- network :: [Int] -> Network
-- network 


-- | feedforward function
-- |  - takes in Network and input(column vector)
-- |  - returns output
feedforward :: Network -> Matrix Float -> Matrix Float
feedforward nw (Matrix inp) = feed inp (biases nw) (weights nw)
    where feed ip [] _ = ip
          feed ip _ [] = ip
          feed ip (b:bs) (w:ws) = feed (sigmoidMat ((w Matrix.* ip) Matrix.+ b)) bs ws

-- | stochastic Gradient Descent
-- |  - takes in: 
-- |        training_data: a tuple (x, y), x being input vector and y being desired output
-- |        epochs: number of epochs
-- |        minibatch_size: size of each minibatch as training all at once is slow
-- |        eta: learning rate
-- |        test_data: this is optional
--stochasticGD :: (Matrix m, Matrix m) -> Int -> Int -> Float -> Maybe (Matrix m, Matrix m) -> Network
--stochasticGD tr_d ep mb_sz et tst_d = runepochs ep


-- TODO: first write non dirty functions(SGD is dirty as it uses random)
--  evaluate, back_prop, cost_derivative, sigmoid'(derivative), update_mini_batch

-- | costDerivative
-- |  - returns the vector of partial derivatives for the output activations
costDerivative :: Matrix Float -> Matrix Float -> Matrix Float
costDerivative outputActivations y = outputActivations Matrix.- y

-- | evaluate
-- |  - returns the number of test inputs for which the output is correct
evaluate :: (Matrix Float, Matrix Float) -> Network -> Int
evaluate (inputs, outputs) nw = V.sum $ V.map (\(x,y)-> boolToInt (x==y)) result
    where boolToInt | True = 1 :: Int
                    | False = 0 :: Int
          result = V.map desiredActualTuple $ V.zip inputs outputs
          desiredActualTuple (ip, op) = (feedforward nw ip, op)

-- | backpropagate
-- |  - takes input and desired output
-- |  - returns tuple grad_b, grad_w representing gradient of C wrt biases and weights
backpropagate :: Network -> Matrix Float -> Matrix Float -> ([Matrix Float], [Matrix Float])
backpropagate network input output = (nabla_b, nabla_w)
    where (zs, activations) = V.foldr calcZ ([],[input]) $ zip (biases network) (weights network)
          calcZ (Z@(z:zs), A@(a:as)) (b, wt) = let wt_sum = (wt Matrix.* a) Matrix.+ b in (wt_sum:Z, sigmoidMat wt_sum:A)
          lastDelta = hadamard (costDerivative (head activations) output) $ sigmoidMat' (head zs)
          (nabla_b, nabla_w) = foldr evalBack (delta, delta Matrix.* (transpose (V.head (V.tail activations)))) $ V.zip3 (V.tail zs) (weights network) (activations) -- TODO
