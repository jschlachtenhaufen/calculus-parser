module Utility where

unique :: [String] -> [String]
unique = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

roundN :: Double -> Double -> Double
roundN f n = (fromInteger $ round $ f * (10 ** n)) / (10.0 ** n)

nthRoot :: (Floating a1, Integral a2) => a2 -> a1 -> a1
nthRoot n x = x ** (1 / fromIntegral n)