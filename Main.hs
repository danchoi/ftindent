{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 

-- Reindents the type signature of Haskell functions.
{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
-- For use within a text editor like Vim.

module Main where
import Text.Parsec
import qualified Text.Parsec.Token as T (GenTokenParser(..))
import Text.Parsec.Language
import qualified Data.Text.IO as T


-- parse :: Stream s Identity t => Parsec s () a -> SourceName -> s -> Either ParseError a

--  main    = case (parse numbers "" "11, 2, 43") of
--             Left err  -> print err
--             Right xs  -> print (sum xs)
-- 
--  numbers = commaSep integer




main :: IO ()
main = do
    s <- getContents
    case (parse numbers "" s) of
            Left err  -> print err
            Right (xs ) -> print . length $ xs

T.TokenParser{..} = haskell

numbers :: Parsec String () [Integer]
-- numbers = (commaSep haskell) (integer haskell)
numbers = commaSep integer 




