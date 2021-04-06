import Data.Char
import Data.Maybe
import Control.Applicative

main :: IO ()
main = print $ span isDigit "876kjfhsgd876kdjfhg"

data JSON = Jnull | Jbool Bool | Jnum Int | Jstring String | Jarray [JSON] | Jobject [(String,JSON)]
   deriving (Show,Eq)

newtype Parser a = Parser {
   parse :: String -> Maybe (String, a)
}
instance Functor Parser where
   fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
   pure x = Parser $ \s -> Just (s,x)
   Parser p1 <*> Parser p2 = Parser p
      where
      p s
         | isNothing (p1 s) = Nothing
         | isNothing (p2 s1) = Nothing
         | otherwise = Just (s2,f x)
         where
         Just (s1,f) = p1 s
         Just (s2,x) = p2 s1

instance Alternative Parser where
   empty = Parser $ const Nothing
   Parser p1 <|> Parser p2 = Parser p
      where
      p s = p1 s <|> p2 s

json :: Parser JSON
json = jnull <|> jbool <|> jnum <|> jstring <|~ jarray

cparse :: Char -> Parser Char
cparse c = Parser f
   where
   f [] = Nothing
   f (x:xs)
      | c == x = Just (xs, c)
      | otherwise = Nothing

sparse :: String -> Parser String
sparse = traverse cparse

jnull :: Parser JSON
jnull = const Jnull <$> sparse "null"

jbool :: Parser JSON
jbool = f <$> (sparse "true" <|> sparse "false")
   where
   f "true" = Jbool True
   f "false" = Jbool False
   f _ = undefined

pspan :: (Char -> Bool) -> Parser String
pspan f = Parser p
   where
   p s = Just (rest,token)
      where
      (token,rest) = span f s

jnum :: Parser JSON
jnum = f <$> psome (pspan isDigit)
   where
   f s = Jnum $ read s

psome :: Parser [a] -> Parser [a]
psome (Parser p) = Parser f
   where
   f s
      | null xs = Nothing
      | otherwise = Just (s',xs)
         where
         Just (s',xs) = p s

sliteral :: Parser String
sliteral = pspan (/= '"')

jstring :: Parser JSON
jstring = Jstring <$> (cparse '"' *> sliteral <* cparse '"')

jarray :: Parser JSON
jarray = Jarray <$> (cparse '[' *> whitespace *> elements <* whitespace <* cparse ']')
   where
   elements = separate (whitespace *> cparse ',' <* whitespace) json

whitespace :: Parser String
whitespace = pspan isSpace

separate :: Parser a -> Parser b -> Parser [b]
separate sep el = (:) <$> el <*> many (sep *> el)
