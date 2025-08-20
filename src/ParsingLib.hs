module ParsingLib where

import Control.Applicative
import Data.List
import Data.Char

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
