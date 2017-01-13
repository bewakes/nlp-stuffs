bigrams :: [a] -> [[a]]
bigrams (x:y:xs) = [[x,y]] ++ bigrams (y:xs)
bigrams _ = []

ngrams :: Int -> [a] -> [[a]]
ngrams n lst | n<1 || length lst < n  = []
            | otherwise = [take n lst] ++ (ngrams n $ tail lst)

skipBigrams :: [a] -> [[a]]
skipBigrams (x:y:xs) = (map (\v-> [x,v]) (y:xs)) ++ skipBigrams (y:xs)
skipBigrams _ = []

main = putStrLn $ show $ ngrams 3 [1,2,3,4,5,6]
