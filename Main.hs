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

data TypeParam = TypeParam String 
          | FuncTypeParam [TypeParam]
        deriving (Show)

parser :: Parsec String () FunctionType
parser = do 
    whiteSpace 
    f <- identifier
    n <- getPosition
    symbol "::" 
    xs <- typeParams
    return $ FunctionType f (sourceColumn n) xs

typeParams :: Parsec String () [TypeParam]
typeParams = sepBy (typeParam <|> functionParam) (symbol "->")

typeParam :: Parsec String () TypeParam
typeParam = TypeParam <$> identifier

functionParam :: Parsec String () TypeParam
functionParam = do
    xs <- between (symbol "(") (symbol ")") typeParams
    return $ FuncTypeParam xs

main :: IO ()
main = do
    s <- getContents
    case (parse parser "" s) of
            Left err  -> print err
            Right (xs ) -> print xs

numbers :: Parsec String () [Integer]
-- numbers = (commaSep haskell) (integer haskell)
numbers = commaSep integer 




