module Network (
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
-- |  - takes in Network and input
-- |  - returns output
feedforward :: Network -> [Float] -> [Float]
feedforward nw inp = feed inp (biases nw) (weights nw)
    where feed ip [] _ = ip
          feed ip _ [] = ip
          feed ip (b:bs) (w:ws) = feed (sigmoidMat ((w Matrix.* ip) Matrix.+ b)) bs ws

--
-- | stochastic Gradient Descent
-- |  - takes in: 
-- |        training_data: a tuple (x, y), x being input vector and y being desired output
-- |        epochs: number of epochs
-- |        minibatch_size: size of each minibatch as training all at once is slow
-- |        eta: learning rate
-- |        test_data: this is optional
stochasticGD :: (Matrix m, Matrix m) -> Int -> Int -> Float -> Maybe (Matrix m, Matrix m) -> Network
stochasticGD tr_d ep mb_sz et tst_d = runepochs ep


-- TODO: first write non dirty functions(SGD is dirty as it uses random)
--  evaluate, back_prop, cost_derivative, sigmoid'(derivative), update_mini_batch
