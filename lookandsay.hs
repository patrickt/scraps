-- http://en.wikipedia.org/wiki/Look-and-say_sequence
-- Implementation of the look-and-say sequence in 15 lines of code.

-- Arrows are a very nice way to represent sequenced, chainable functions
-- in Haskell. For more info, check the wonderful Wikibooks tutorial at
-- http://en.wikibooks.org/wiki/Haskell/Understanding_arrows
import Control.Arrow
import Data.List (group)

-- `say` produces a new integer by reading out the digits in its argument.
-- The >>> operator sequences functions together.
say :: Integer -> Integer
say = digits >>> encode >>> glue

-- digits splits a number up into its component digits.
digits :: Integer -> [Integer]
digits = fmap read . fmap return . show

-- encode performs run-length encoding on its arguments.
-- We get rid of nasty tuple manipulation with the &&& combinator.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = fmap (length &&& head) . group

-- glue assembles the new number out of a list of encoded tuples
glue :: [(Int, Integer)] -> Integer
glue = read . concatMap show' where show' (a,b) = show a ++ show b

-- The iterate method builds an infinite list out of a starting value and method.
lookAndSaySequence :: [Integer]
lookAndSaySequence = iterate say 1

-- Proof that it works.
main :: IO ()
main = print $ take 15 lookAndSaySequence