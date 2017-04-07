module Network (
    Network(..)
)
where

import Matrix (
        Matrix(..)
        , (*)
        , (+)
        , (-)
        , hadamard
        , transpose
        , zeroMatrix
        , zeroMatrixBySize
        , size
        , scale
    )
import Utilities(sigmoidMat, sigmoidMat', randomMatrixBySize, listToMonad, shuffle)
import Data.Vector as V
import Prelude as P
import System.Random


data Network = Network {
    layerSizes :: [Int],
    biases :: [Matrix Float],
    weights :: [Matrix Float],
    numLayers :: Int
} deriving (Show)

-- | Network constructor,  takes in layerSizes, initializes biases and weights
-- |    returns IO Network (because of random numbers
network :: [Int] -> IO Network
network neuroList = fmap cons ioTupleWtsBias
    where cons (wts, bias) = Network
                {
                    layerSizes = neuroList
                    , biases = bias
                    , weights = wts
                    , numLayers = P.length neuroList
                }
          wtSizes = P.zip (P.tail neuroList) (neuroList)
          weights = listToMonad (P.map randomMatrixBySize wtSizes)
          biasSzs = P.zip (P.tail neuroList) [1,1..]
          biases = listToMonad (P.map randomMatrixBySize biasSzs)
          ioTupleWtsBias = do
            w <- weights
            b <- biases
            return (w, b)


-- | feedforward function
-- |  - takes in Network and input(column vector)
-- |  - returns output
feedforward :: Network -> Matrix Float -> Matrix Float
feedforward nw inpmat = feed inpmat (biases nw) (weights nw)
    where feed ip [] _ = ip
          feed ip _ [] = ip
          feed ip (b:bs) (w:ws) = feed (sigmoidMat ((w Matrix.* ip) Matrix.+ b)) bs ws

-- | stochastic Gradient Descent
-- |  - takes in: 
-- |        network: network where training data is applied
-- |        training_data: a tuple (x, y), x being input vector and y being desired output
-- |        epochs: number of epochs
-- |        minibatch_size: size of each minibatch as training all at once is slow
-- |        eta: learning rate
-- |        test_data: this is optional
stochasticGD :: Network -> [(Matrix Float, Matrix Float)] -> Int -> Int -> Float -> Maybe [(Matrix Float, Matrix Float)] -> IO Network
stochasticGD nw tr_d ep mb_sz et tst_d = runepochs nw ep
    where runepochs nt 0 = return nt
          runepochs nt e = do
            shuffled <- shuffle tr_d
            batches <- return (splitBySize shuffled mb_sz)
            net <- return $ P.foldl (\nn bat -> updateMiniBatch nn bat et) nt batches
            evaluate_test tst_d net
            runepochs net (e P.- 1)
          evaluate_test testdata nn = if testdata == Nothing || (maybeToVal testdata) == [] then return ".." else do
            d <- return $ maybeToVal testdata
            n <- return $ evaluate d nn
            putStrLn$ show n P.++ (show (P.length d))
            return ""
          splitBySize [] _ = []
          splitBySize l n = (P.take n l) : (splitBySize (P.drop n l) n)
          maybeToVal Nothing = []
          maybeToVal (Just x) = x



-- | updateMiniBatch
-- |  - update the weights and biases of the network applying backpropagation
-- |  - takes in network, miniBatch, eta
-- |  - returns updated network
updateMiniBatch :: Network -> [(Matrix Float, Matrix Float)] -> Float -> Network
updateMiniBatch network miniBatch eta = let
            wts = weights network
            bss = biases network
            zwts = P.map (zeroMatrixBySize . size) wts -- wts zero
            zbis = P.map (zeroMatrixBySize . size) bss -- biases zero
            (nb, nw) = P.foldl sumWtsBiases (zbis, zwts) $ P.map (\(x,y)->backpropagate network x y) miniBatch
            addfunc = (\(x,y) -> x Matrix.+ y)
            sumWtsBiases (zb,zw) (b,w) = (P.map addfunc (P.zip zb b), P.map addfunc  (P.zip zw w))
            new_wts = P.map (\(x,y) -> x Matrix.- (scale y (eta / batchLen))) $ P.zip wts nw
            new_bss = P.map (\(x,y) -> x Matrix.- (scale y (eta / batchLen))) $ P.zip bss nb
            batchLen = fromIntegral $ P.length miniBatch
        in network {
            weights = new_wts
            , biases = new_bss
        }


-- | costDerivative
-- |  - returns the vector of partial derivatives for the output activations
costDerivative :: Matrix Float -> Matrix Float -> Matrix Float
costDerivative outputActivations y = outputActivations Matrix.- y

-- | evaluate
-- |  - returns the number of test inputs for which the output is correct
evaluate :: [(Matrix Float, Matrix Float)] -> Network -> Int
evaluate list nw = P.sum $ P.map (\(x,y)-> boolToInt (x==y)) result
    where boolToInt x = if x==True then 1 else 0
          result = P.map desiredActualTuple list
          desiredActualTuple (ip, op) = (feedforward nw ip, op)

-- | backpropagate
-- |  - takes input and desired output
-- |  - returns tuple grad_b, grad_w representing gradient of C wrt biases and weights
backpropagate :: Network -> Matrix Float -> Matrix Float -> ([Matrix Float], [Matrix Float])
backpropagate network input output = (nabla_b, nabla_w)
    where (zs, activations) = P.foldl calcZ ([],[input]) $ P.zip (biases network) (weights network)

          calcZ ( (z:zs), (a:as)) (b, wt) = let wt_sum = (wt Matrix.* a) Matrix.+ b in (wt_sum:(z:zs), (sigmoidMat wt_sum):(a:as))


          lastDelta = hadamard (costDerivative (P.head activations) output) $ sigmoidMat' (P.head zs)
          (nabla_b, nabla_w) = P.foldl evalBack ([lastDelta], [last_nabla_w]) $ P.zip3 (P.tail zs) wts actvns

          wts = P.reverse (weights network)
          -- both zs and activations are in reverse but weights are not, so reversing weights
          actvns = P.tail activations

          last_nabla_w = lastDelta Matrix.* secondLastActv'
          secondLastActv' = transpose (P.head (P.tail activations))

          evalBack ((nb:nbs), (nw:nws)) (z, w, a) = let
                n_b = hadamard ((transpose w) Matrix.* nb) (sigmoidMat' z)
                n_w = n_b Matrix.* (transpose a)
            in(n_b:(nb:nbs), n_w:(nw:nws))
