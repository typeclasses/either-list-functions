{-# LANGUAGE NoImplicitPrelude #-}

module Data.List.EitherFunctions
  ( partlyMap
  , groupEither
  , spanLeft
  , spanRight
  ) where

import Data.Either (Either (..))
import Data.List   (map)
import Data.Maybe  (Maybe (..), maybe)

{- |

>>> import Prelude (even, show)

>>> partlyMap (\x -> if even x then Just (show x) else Nothing) [1..5]
[Left 1,Right "2",Left 3,Right "4",Left 5]

-}
partlyMap :: (a -> Maybe b) -> [a] -> [Either a b]
partlyMap f = map (\x -> maybe (Left x) Right (f x))

{- |

>>> groupEither [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
[Left [1,2],Right "a",Left [3],Right "bc"]

-}
groupEither :: [Either a b] -> [Either [a] [b]]
groupEither [] = []
groupEither ((Left x) : xs)  = let (ys, zs) = spanLeft xs
                               in  Left (x : ys) : groupEither zs
groupEither ((Right x) : xs) = let (ys, zs) = spanRight xs
                               in  Right (x : ys) : groupEither zs

{- |

>>> spanLeft [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
([1,2],[Right 'a',Left 3,Right 'b',Right 'c'])

>>> spanLeft [Right 'a', Left 3, Right 'b', Right 'c']
([],[Right 'a',Left 3,Right 'b',Right 'c'])

-}
spanLeft :: [Either a b] -> ([a], [Either a b])
spanLeft [] = ([], [])
spanLeft ((Left x) : xs) = let (ys, zs) = spanLeft xs
                           in  (x : ys, zs)
spanLeft xs = ([], xs)

{- |

>>> spanRight [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
("",[Left 1,Left 2,Right 'a',Left 3,Right 'b',Right 'c'])

>>> spanRight [Right 'a', Left 3, Right 'b', Right 'c']
("a",[Left 3,Right 'b',Right 'c'])

-}
spanRight :: [Either a b] -> ([b], [Either a b])
spanRight [] = ([], [])
spanRight ((Right x) : xs) = let (ys, zs) = spanRight xs
                             in  (x : ys, zs)
spanRight xs = ([], xs)
