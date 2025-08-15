
module Main where

import Data.Ratio

data ReducedSqrt = Irrational (Ratio Integer) (Ratio Integer)
                 | Perfect (Ratio Integer)
                 | Sum ReducedSqrt ReducedSqrt

instance Show ReducedSqrt where

    show (Irrational a b) = show a ++ " sqrt(" ++ show b ++ ")"
    show (Perfect a)      = show a
    show (Sum a b)        = show a ++ " + " ++ show b


--TODO rename
-- converts an rl encoding of an integers prime factors to an integer
rlEncode :: Eq a => [a] -> [(Integer, a)]
rlEncode list = map (\x -> (toInteger $ length x, head x) ) $ pack list

rlToNum :: [(Integer, Integer)] -> Integer
rlToNum [] = 1
rlToNum ((_, prime):xs) = prime * rlToNum xs

pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:takeWhile (==x) xs): pack (dropWhile (==x) xs)

primeFactors :: Integer -> [Integer]
primeFactors 1 = [1]
primeFactors n = aux n [2..n]
  where
    aux a [] = []
    aux 1 _  = []
    aux n l@(x:xs)
      | mod n x == 0 = x : aux (div n x) l
      | otherwise     = aux n xs

primeFactorMultiplicity :: Integer -> [(Integer, Integer)]
primeFactorMultiplicity = rlEncode . primeFactors

reduceInnerSqrt :: Integer -> Integer
reduceInnerSqrt a = go (primeFactorMultiplicity a)
  where
    go ls = rlToNum $ filter (\(mult, prime) -> odd mult) ls

reduceOuterSqrt :: Integer -> Integer
reduceOuterSqrt a = rlToNumSquares $ clean (primeFactorMultiplicity a)
  where
    clean = filter (\(mult, prime) -> mult > 1)
    rlToNumSquares [] = 1
    rlToNumSquares ((mult, prime):rest)
      | even mult = prime ^ div mult 2 * rlToNumSquares rest
      | otherwise = prime ^ div (mult - 1) 2 * rlToNumSquares rest

intToReducedSqrt :: Ratio Integer -> ReducedSqrt
intToReducedSqrt x =
  case (reduceInnerSqrt $ numerator x, reduceInnerSqrt $ denominator x) of
    (1, 1) -> Perfect $ numOuterReduce % denOuterReduce
    (1, simple) -> Irrational (numOuterReduce % denOuterReduce)
                              ( 1 % simple)
    (simple, 1) -> Irrational (numOuterReduce % denOuterReduce)
                              (simple % 1)
    (simpleN, simpleD) -> Irrational (numOuterReduce % reduceOuterSqrt (denominator x))
                                     (simpleN % simpleD)
    where numOuterReduce = reduceOuterSqrt (numerator x)
          denOuterReduce = reduceOuterSqrt (denominator x)


main = return ()
