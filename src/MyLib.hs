module MyLib where

import Data.Char
import Data.List
import Data.Ratio
import Control.Applicative

data ReducedSqrt = Irrational Integer Integer
                 | Perfect Integer
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
    Sum a b -> error "indeterminate, implement later"

  signum (Perfect a) = Perfect $ signum a
  signum (Irrational a _) = Perfect $ signum a
  signum (Sum a b) = error "indeterminant, implment later"

  fromInteger = Perfect

instance Show ReducedSqrt where

    show (Irrational a b) = show a ++ " sqrt(" ++ show b ++ ")"
    show (Perfect a)      = show a
    show (Sum a b)        = show a ++ " + " ++ show b

data Expr a
  = Reduced a
  | Add   (Expr a) (Expr a)
  | Sub   (Expr a) (Expr a)
  | Mult  (Expr a) (Expr a)
  | Div   (Expr a) (Expr a)
  | Exp   (Expr a) (Expr a)
  | Negate (Expr a)
  | Sqrt  (Expr a)
  | ExEmpty
  deriving (Show, Eq)

digitsCharP :: Parser (Expr Int)
digitsCharP = f <$> spanP isDigit
  where f ds = Reduced $ read ds

satisfy :: (Char -> Bool) -> Parser Char
satisfy charBool = Parser f
  where
    f (x:xs)
      | charBool x = Just (x, xs)
      | otherwise  = Nothing
    f [] = Nothing

digitCharP :: Parser Char
digitCharP = satisfy isDigit

someDigitsP :: Parser String
someDigitsP = some digitCharP

numberParser :: Parser Integer
numberParser = fmap read someDigitsP

signParser :: Parser (Integer -> Integer)
signParser = negate <$ charP '-' <|> pure id

integerP :: Parser Integer
integerP = signParser <*> (read <$> someDigitsP)

intExprP :: Parser (Expr Integer)
intExprP = fmap Reduced integerP

exprP :: Parser (Expr Integer)
exprP = termP `chainl1` (addOp <|> subOp)


parenP :: Parser (Expr Integer)
parenP = charP '(' *>  ws *>
                exprP
                <* ws <* charP ')'

sqrtP :: Parser (Expr Integer)
sqrtP = Sqrt <$> (stringP "sqrt(" *> ws *>
                  exprP
                 <* ws <* charP ')')

addOp :: Parser (Expr a -> Expr a -> Expr a)
addOp = Add <$ token (charP '+')

subOp :: Parser (Expr a -> Expr a -> Expr a)
subOp = Sub <$ token (charP '-')

mulOp :: Parser (Expr a -> Expr a -> Expr a)
mulOp = Mult <$ token (charP '*')

divOp :: Parser (Expr a -> Expr a -> Expr a)
divOp = Div <$ token (charP '/')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p1 p2 = do
  a <- p1
  rest a
  where
    rest acc =
      (do f <- p2
          b <- p1
          rest (f acc b))
      <|> return acc

termP :: Parser (Expr Integer)
termP = factorP `chainl1` (mulOp <|> divOp)

token :: Parser a -> Parser a
token p = ws *> p <* ws

baseP :: Parser (Expr Integer)
baseP = token intExprP <|> token parenP <|> token sqrtP

factorP :: Parser (Expr Integer)
factorP =
  baseP >>= \b -> (token(charP '^') >>= \_ ->
                    factorP >>= \f ->
                      return (Exp b f))
                  <|> return b

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ span f input

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (x, input') <- p input
      Just (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (f, input') <- p1 input
      (a, input'') <- p2 input'
      Just (f a, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  Parser p >>= f = Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (a, rest) -> runParser (f a) rest
  return = pure

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (x, ys)
    f _ = Nothing

stringP :: String -> Parser String
stringP = traverse charP

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

addR :: ReducedSqrt -> ReducedSqrt -> ReducedSqrt
addR a b = simplifySum (collectTerms (flattenSum [a, b]))

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
      in (if total /= 0 then [Perfect total] else []) ++ rest

    combineIrrationals xs = foldr insertIrrational [] xs

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

intToReducedSqrt :: Integer -> ReducedSqrt
intToReducedSqrt x
  | reduceInnerSqrt x == 1 = Perfect $ reduceOuterSqrt x
  | otherwise              = Irrational (reduceOuterSqrt x) (reduceInnerSqrt x)


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
eval (Reduced x) = Perfect x
eval (Sqrt (Reduced a)) = intToReducedSqrt a
eval (Sqrt e) =
  case eval e of
    Perfect x -> intToReducedSqrt x
    other     -> Irrational 1 (toInnerSqrt other)
eval (Add a b) = addR (eval a) (eval b)
eval (Sub a b) = subR (eval a) (eval b)
eval (Mult a b) = rSqrtMult (eval a) (eval b)
eval (Div a b) =
  case (eval a, eval b) of
    (Perfect m, Perfect n)
      | n /= 0 -> Perfect (m `div` n)
      | otherwise -> Sum (Perfect m) (Perfect n)  -- fallback for div-by-zero
    (x, y) -> Sum x y  -- symbolic fallback
eval (Exp base power) =
  case (eval base, eval power) of
    (Perfect b, Perfect e) -> Perfect (b ^ e)
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
reducedSqrtToExpr (Perfect x) = Reduced x
reducedSqrtToExpr (Irrational a b) = Mult (Reduced a) (Sqrt (Reduced b))
reducedSqrtToExpr (Sum x y) = Add (reducedSqrtToExpr x) (reducedSqrtToExpr y)

toInnerSqrt :: ReducedSqrt -> Integer
toInnerSqrt (Perfect x) = x
toInnerSqrt (Irrational _ b) = b
toInnerSqrt (Sum _ _) = 0  -- fallback for now (cold raise symbolic sqrt expression)


evalPrint :: Maybe ReducedSqrt
evalPrint = (eval.fst) <$> test

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
