module MyLib where

import ParsingLib
import Data.Char
import Data.List
import Data.Ratio ( Ratio, (%), denominator, numerator )
import Control.Applicative

data ReducedSqrt = Irrational (Ratio Integer) (Ratio Integer)
                 | Perfect (Ratio Integer)
                 | Sum ReducedSqrt ReducedSqrt

instance Num ReducedSqrt where
  (+) = addR
  (*) = rSqrtMult

  negate (Irrational a b) = Irrational (negate a) b
  negate (Perfect a)      = Perfect $ negate a
  negate (Sum a b)        = Sum (negate a) (negate b)

  abs (Perfect a) = Perfect (abs a)
  abs (Irrational a _) = Perfect (abs a)
  abs x@(Sum _ _) = case simplifySum $ collectTerms $ flattenSum [x] of
    Perfect a -> abs $ Perfect a
    Irrational a b -> abs $ Irrational a b
    Sum a b -> error "indeterminate, implement later" --TODO implement

  signum (Perfect a) = Perfect $ signum a
  signum (Irrational a _) = Perfect $ signum a
  signum (Sum a b) = error "implement later"

  fromInteger x = Perfect (x % 1)

addR :: ReducedSqrt -> ReducedSqrt -> ReducedSqrt
addR a b = simplifySum (collectTerms (flattenSum [a, b]))

instance Show ReducedSqrt where

    show (Irrational a b) =
      case (denominator a, denominator b) of
      (1, 1) -> sqrtShow (numerator a) (numerator b)
      (1, _) -> sqrtShow (numerator a) b
      (_, 1) -> sqrtShow a (numerator b)
      (_, _) -> sqrtShow a b
      where
        sqrtShow outer inner = show outer ++ " sqrt(" ++ show inner ++ ")"

    show (Perfect a)
      | denominator a == 1  = show $ numerator a
      | otherwise         = show a

    show (Sum a b)        = show a ++ " + " ++ show b

flattenSum :: [ReducedSqrt] -> [ReducedSqrt]
flattenSum = concatMap go
  where
    go (Sum x y) = flattenSum [x, y]
    go x         = [x]

collectTerms :: [ReducedSqrt] -> [ReducedSqrt]
collectTerms = combinePerfects . combineIrrationals
  where
    combinePerfects xs =
      let (perfects, rest) = partitionPerfect xs
          total = sum [n | Perfect n <- perfects]
      in [Perfect total | total /= 0] ++ rest

    combineIrrationals = foldr insertIrrational []

    insertIrrational (Irrational a r) acc =
      let (same, others) = span (\x -> case x of Irrational _ r' -> r == r'; _ -> False) acc
          coeffSum = a + sum [c | Irrational c _ <- same]
      in if coeffSum /= 0
         then Irrational coeffSum r : others
         else others
    insertIrrational x acc = x : acc

    partitionPerfect = partition isPerfect
    isPerfect (Perfect _) = True
    isPerfect _ = False

simplifySum :: [ReducedSqrt] -> ReducedSqrt
simplifySum [] = Perfect 0
simplifySum [x] = x
simplifySum (x:y:rest) = simplifySum (Sum x y : rest)

subR :: ReducedSqrt -> ReducedSqrt -> ReducedSqrt
subR a b =  addR a (-b)

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

rSqrtMult :: ReducedSqrt -> ReducedSqrt -> ReducedSqrt
rSqrtMult (Perfect a) (Perfect b) = Perfect (a*b)
rSqrtMult (Perfect a) (Irrational outer inner) = Irrational (a*outer) inner
rSqrtMult (Irrational outer inner) (Perfect b) = Irrational (b*outer) inner
rSqrtMult (Irrational a b) (Irrational x y)    = intToReducedSqrt (a*a*x*x*b*y)
-- add sum multiplication
rSqrtMult l@(Perfect a) l2@(Sum _ _) =
  simplifySum $ collectTerms $ distributiveMult [l] (flattenSum [l2])
rSqrtMult l2@(Sum _ _) l@(Perfect a) =
  simplifySum $ collectTerms $ distributiveMult [l] (flattenSum [l2])
rSqrtMult l@(Irrational a b) l2@(Sum _ _) =
  simplifySum $ collectTerms $ distributiveMult [l] (flattenSum [l2])
rSqrtMult l2@(Sum _ _) l@(Irrational a b) =
  simplifySum $ collectTerms $ distributiveMult [l] (flattenSum [l2])
rSqrtMult l1@(Sum _ _) l2@(Sum _ _)
  | length (flattenSum [l1]) >= length (flattenSum [l2]) =
    simplifySum $ collectTerms $ distributiveMult [l1] (flattenSum [l2])
  | otherwise = simplifySum $ collectTerms $ distributiveMult [l2] (flattenSum [l1])

eval :: Expr Integer -> ReducedSqrt
eval (Reduced x) = Perfect $ x % 1
eval (Sqrt (Reduced a)) = intToReducedSqrt (a % 1)
eval (Sqrt e) =
  case eval e of
    Perfect x -> intToReducedSqrt x
    other     -> Irrational 1 (toInnerSqrt other % 1)
eval (Add a b) = addR (eval a) (eval b)
eval (Sub a b) = subR (eval a) (eval b)
eval (Mult a b) = rSqrtMult (eval a) (eval b)
eval (Div a b) =
  case (eval a, eval b) of
    -- TODO write integeral for ReducedSqrt
    (Perfect m, Perfect n)
      -- | n /= 0 -> Perfect (m `div` n)
      | n /= 0 -> Perfect (m / n)
      | otherwise -> error "Division by zero"  -- fallback for div-by-zero
    (x, y) -> Sum x y  -- symbolic fallback
eval (Exp base power) =
  case (eval base, eval power) of
    (Perfect b, Perfect e) ->
      case denominator e of
        1 -> Perfect $ numerator b^numerator e % denominator b^numerator e -- handle integer powers with rational base
        _ -> error "wut"-- fallback, TODO add rational exponent support
    (b, Perfect e)
      | e == 2 -> rSqrtMult b b
      | otherwise -> Sum b (Perfect e)  -- symbolic fallback
    (b, e) -> Sum b e  -- general symbolic fallback
eval (Negate e) =
  case eval e of
    Perfect x -> Perfect (-x)
    Irrational a b -> Irrational (-a) b
    Sum x y -> Sum (eval (Negate (reducedSqrtToExpr x)))
                   (eval (Negate (reducedSqrtToExpr y)))

fromExpr :: Expr ReducedSqrt -> ReducedSqrt
fromExpr (Reduced a) = a
fromExpr _ = error "fromExpr error"

-- Helper to convert ReducedSqrt back to Expr for symbolic use (e.g. in Negate)
reducedSqrtToExpr :: ReducedSqrt -> Expr Integer
reducedSqrtToExpr (Perfect x) = Reduced $ numerator x
reducedSqrtToExpr (Irrational a b) = Mult (Reduced $ numerator a) (Sqrt (Reduced $ numerator b))
reducedSqrtToExpr (Sum x y) = Add (reducedSqrtToExpr x) (reducedSqrtToExpr y)

toInnerSqrt :: ReducedSqrt -> Integer
toInnerSqrt (Perfect x) = numerator x
toInnerSqrt (Irrational _ b) = numerator b
toInnerSqrt (Sum _ _) = 0  -- fallback for now (cold raise symbolic sqrt expression)


evalPrint :: Maybe ReducedSqrt
evalPrint = eval.fst <$> test

test1 = Add (Reduced (Perfect 3)) (Reduced (Perfect 5))

test = runParser exprP "sqrt(2) + sqrt(5) + sqrt(11)"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

distributiveMult :: Num a => [a] -> [a] -> [a]
distributiveMult [] [] = []
distributiveMult a b
  | null a || null b = error "Normalize: longer sum first or empty term match"
distributiveMult [a] [b] = [a*b]
distributiveMult [x]  a  = map (* x) a
distributiveMult (x:xs) a =
     concatMap (distributiveMult [x].(:[])) a
  ++ distributiveMult xs a
