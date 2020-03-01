{-# LANGUAGE BlockArguments, LambdaCase, NoImplicitPrelude #-}

module Data.List.EitherFunctions
  ( partlyMap
  , groupEither
  , spanLeft, spanLeft'
  , spanRight, spanRight'
  , partition
  ) where

import Data.Either   ( Either (..) )
import Data.Function ( fix )
import Data.List     ( map )
import Data.Maybe    ( Maybe (..), maybe )

-- |
-- >>> import Prelude (even, show)
--
-- >>> partlyMap (\x -> if even x then Just (show x) else Nothing) [1..5]
-- [Left 1,Right "2",Left 3,Right "4",Left 5]

partlyMap :: (a -> Maybe b) -> [a] -> [Either a b]

partlyMap f = map \x -> maybe (Left x) Right (f x)

-- |
-- >>> groupEither [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- [Left [1,2],Right "a",Left [3],Right "bc"]

groupEither :: [Either a b] -> [Either [a] [b]]

groupEither = fix \r -> \case
    []            ->  []
    Left  x : xs  ->  Left  (x : ys) : r zs  where (ys, zs) = spanLeft  xs
    Right x : xs  ->  Right (x : ys) : r zs  where (ys, zs) = spanRight xs

-- |
-- >>> spanLeft [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- ([1,2],[Right 'a',Left 3,Right 'b',Right 'c'])
--
-- >>> spanLeft [Right 'a', Left 3, Right 'b', Right 'c']
-- ([],[Right 'a',Left 3,Right 'b',Right 'c'])

spanLeft :: [Either a b] -> ([a], [Either a b])

spanLeft = fix \r -> \case
    []           ->  ( []     , [] )
    Left x : xs  ->  ( x : ys , zs )  where (ys, zs) = r xs
    xs           ->  ( []     , xs )

-- | Similar to 'spanLeft', but preserves a little more information in the return type: if the remainder of the list is non-empty, then it necessarily begins with a 'Right', and so we can go ahead and unwrap that and return it as a value of type `b`.
--
-- >>> spanLeft' [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- ([1,2],Just ('a',[Left 3,Right 'b',Right 'c']))
--
-- >>> spanLeft' [Right 'a', Left 3, Right 'b', Right 'c']
-- ([],Just ('a',[Left 3,Right 'b',Right 'c']))
--
-- >>> spanLeft' [Left 1, Left 2, Left 3]
-- ([1,2,3],Nothing)

spanLeft' :: [Either a b] -> ([a], Maybe (b, [Either a b]))

spanLeft' = fix \r -> \case
    []            ->  ( []     , Nothing      )
    Left  x : xs  ->  ( x : ys , zs           )  where (ys, zs) = r xs
    Right x : xs  ->  ( []     , Just (x, xs) )

-- | Similar to 'spanRight', but preserves a little more information in the return type: if the remainder of the list is non-empty, then it necessarily begins with a 'Left', and so we can go ahead and unwrap that and return it as a value of type `a`.
--
-- >>> spanRight [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- ("",[Left 1,Left 2,Right 'a',Left 3,Right 'b',Right 'c'])
--
-- >>> spanRight [Right 'a', Left 3, Right 'b', Right 'c']
-- ("a",[Left 3,Right 'b',Right 'c'])

spanRight :: [Either a b] -> ([b], [Either a b])

spanRight = fix \r -> \case
    []            ->  ( []     , [] )
    Right x : xs  ->  ( x : ys , zs )  where (ys, zs) = r xs
    xs            ->  ( []     , xs )

-- |
-- >>> spanRight' [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- ("",Just (1,[Left 2,Right 'a',Left 3,Right 'b',Right 'c']))
--
-- >>> spanRight' [Right 'a', Left 3, Right 'b', Right 'c']
-- ("a",Just (3,[Right 'b',Right 'c']))
--
-- >>> spanRight' [Right 'a', Right 'b', Right 'c']
-- ("abc",Nothing)

spanRight' :: [Either a b] -> ([b], Maybe (a, [Either a b]))

spanRight' = fix \r -> \case
    []            ->  ( []     , Nothing      )
    Right x : xs  ->  ( x : ys , zs           )  where (ys, zs) = r xs
    Left  x : xs  ->  ( []     , Just (x, xs) )

-- |
-- >>> partition [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- ([1,2,3],"abc")

partition :: [Either a b] -> ([a], [b])

partition = fix \r -> \case
    []            ->  ([], [])
    Left  a : xs  ->  (a : as, bs)  where (as, bs) = r xs
    Right b : xs  ->  (as, b : bs)  where (as, bs) = r xs
