module AocParse
  ( Parser (..),
    matchChar,
    matchDigit,
    matchString,
    matchWhile,
    matchInteger,
    matchWhiteSpace,
    matchTo,
    matchLine,
    wrappedBy,
    wrappedByChar,
    discardToNextInteger,
    splitOn,
  )
where

import Control.Applicative
import Data.Char
import Data.Tuple

newtype Parser a = Parser
  {runParser :: String -> Either String (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', a) <- p input
      Right (input', f a)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)

  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', x) <- p2 input'
    Right (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Right (input', a) -> Right (input', a)
    Left _ -> p2 input

-- | Parse a single character
matchChar :: Char -> Parser Char
matchChar c = Parser f
  where
    f (y : ys)
      | y == c = Right (ys, c)
      | otherwise = Left $ "Could not parse `" ++ [c] ++ "` from `" ++ (y : ys) ++ "`"
    f [] = Left $ "Could not parse `" ++ [c] ++ "` from empty input"

matchDigit :: Parser Char
matchDigit = Parser f
  where
    f (y : ys)
      | isDigit y = Right (ys, y)
      | otherwise = Left $ "`" ++ [y] ++ "` is not a digit"
    f [] = Left "Could not parse digit from empty input"

-- | Parse a string
matchString :: String -> Parser String
matchString = traverse matchChar -- traverse = sequenceA $ map matchChar

-- | Parse while predicate holds
matchWhile :: (Char -> Bool) -> Parser String
matchWhile f = Parser g
  where
    g [] = Left "Cannot match on empty input" -- maybe remove this
    g input = Right $ swap (span f input)

-- | Ensure that parsed value is not empty
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Left "Parser returned nothing"
    else Right (input', xs)

-- | Parse an Integer
matchInteger :: Parser Integer
matchInteger = read <$> notNull (matchWhile isDigit)

-- | Parse something wrapped by a character. Wrapping characters are discarded e.g. runParser (wrappedByChar '\'' matchInteger) "'123'" = Right ("", 123)
wrappedByChar :: Char -> Parser a -> Parser a
wrappedByChar c p = matchChar c *> p <* matchChar c

-- | Parse something wrapped by a string. Wrapping strings are discarded e.g. runParser (wrappedBy "hi" matchInteger) "hi123hi" = Right ("", 123)
wrappedBy :: String -> Parser a -> Parser a
wrappedBy s p = matchString s *> p <* matchString s

-- | Parse until next non-white space character
matchWhiteSpace :: Parser String
matchWhiteSpace = matchWhile isSpace

-- | Parse a whole line
matchLine :: Parser String
matchLine = matchWhile (/= '\n') <* matchChar '\n'

-- | Parse the next intger, discarding all characters up to that integer
discardToNextInteger :: Parser Integer
discardToNextInteger = matchWhile (not . isDigit) *> matchInteger

-- | Return list of parsed elements seperated by a parser
splitOn :: Parser a -> Parser b -> Parser [b]
splitOn sep element = (:) <$> element <*> many (sep *> element) -- maybe add <|> pure []

-- | Parse all characters until specified character is found
matchTo :: Char -> Parser String
matchTo c = matchWhile (/= c)
