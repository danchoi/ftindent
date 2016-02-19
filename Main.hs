{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 

-- Reindents the type signature of Haskell functions.
{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
-- For use within a text editor like Vim.

module Main where
import Text.Parsec
import qualified Text.Parsec.Token as T (GenTokenParser(..))
import Text.Parsec.Language
import qualified Data.Text.IO as T
import Data.Monoid
import Data.List (intersperse)

-- parse :: Stream s Identity t => Parsec s () a -> SourceName -> s -> Either ParseError a

--  main    = case (parse numbers "" "11, 2, 43") of
--             Left err  -> print err
--             Right xs  -> print (sum xs)
-- 
--  numbers = commaSep integer

-- type signature parsing
{-
type Identifier = String

data DoubleColon = DoubleColon Int   -- ^ position

data TypeParam = TypeParam Identifier   (Maybe Comment)
               | FuncTypeParam [TypeParam]

data Function = Function Identifier DoubleColon [TypeParam]
-}

T.TokenParser{..} = haskell

data FunctionType = FunctionType String Int [TypeParam] 
    deriving (Show)

type Comment = Maybe String

data TypeParam = 
            TypeParam String Comment
          | FuncTypeParam [TypeParam] Comment Int
        deriving (Show)

testparser :: Parsec String () (String, Int, Int, Maybe String)
testparser = do 
    whiteSpace 
    f <- identifier
    n <- sourceColumn <$> getPosition
    symbol "::" 
    x <- identifier'
    n2 <- sourceColumn <$> getPosition
    c <- comment
    return (x, n,n2, c)

identStart = letter
identLetter = alphaNum <|> oneOf "_'"


-- parses identifiers but does not consume following comment whitespace
identifier' :: Parsec String () String
identifier' = do
    x <- identStart 
    xs <- many identLetter
    spaces
    return (x:xs)

parser :: Parsec String () FunctionType
parser = do 
    whiteSpace 
    f <- identifier
    n <- getPosition
    symbol "::" 
    xs <- typeParams
    return $ FunctionType f (sourceColumn n) xs

typeParams :: Parsec String () [TypeParam]
typeParams = sepBy (typeParam <|> functionParam) (symbol "->" )

typeParam :: Parsec String () TypeParam
typeParam = do
    t <- (concat . intersperse " ")  <$> (many1 identifier')
    spaces
    c <- (comment <* spaces)
    return $ TypeParam t c

comment :: Parsec String () (Maybe String)
comment = do
  option Nothing
    $ fmap Just  (try $ do
      string "--"
      manyTill anyChar (string "\n")
      )

getCol = sourceColumn <$> getPosition

functionParam :: Parsec String () TypeParam
functionParam = do
    xs <- between (string "(") (string ")") typeParams
    n <- getCol
    spaces
    c <- comment 
    return $ FuncTypeParam xs c n

main :: IO ()
main = do
    s <- getContents
    case (parse parser "" s) of
            Left err  -> print err
            Right (xs ) -> print xs

numbers :: Parsec String () [Integer]
-- numbers = (commaSep haskell) (integer haskell)
numbers = commaSep integer 




