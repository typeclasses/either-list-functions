{-# LANGUAGE NoImplicitPrelude #-}

module Data.List.EitherFunctions
  ( partlyMap
  , groupEither
  , spanLeft, spanLeft'
  , spanRight, spanRight'
  , partition
  ) where

import Data.Either (Either (..))
import Data.Maybe  (Maybe (..), maybe)

import qualified Data.List as List

{- |

>>> import Prelude (even, show)

>>> partlyMap (\x -> if even x then Just (show x) else Nothing) [1..5]
[Left 1,Right "2",Left 3,Right "4",Left 5]

-}
partlyMap :: (a -> Maybe b) -> [a] -> [Either a b]
partlyMap f = List.map (\x -> maybe (Left x) Right (f x))

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

>>> spanLeft' [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
([1,2],Just ('a',[Left 3,Right 'b',Right 'c']))

>>> spanLeft' [Right 'a', Left 3, Right 'b', Right 'c']
([],Just ('a',[Left 3,Right 'b',Right 'c']))

>>> spanLeft' [Left 1, Left 2, Left 3]
([1,2,3],Nothing)

-}
spanLeft' :: [Either a b] -> ([a], Maybe (b, [Either a b]))
spanLeft' [] = ([], Nothing)
spanLeft' ((Left x) : xs) = let (ys, zs) = spanLeft' xs
                            in  (x : ys, zs)
spanLeft' ((Right x) : xs) = ([], Just (x, xs))

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

{- |

>>> spanRight' [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
("",Just (1,[Left 2,Right 'a',Left 3,Right 'b',Right 'c']))

>>> spanRight' [Right 'a', Left 3, Right 'b', Right 'c']
("a",Just (3,[Right 'b',Right 'c']))

>>> spanRight' [Right 'a', Right 'b', Right 'c']
("abc",Nothing)

-}
spanRight' :: [Either a b] -> ([b], Maybe (a, [Either a b]))
spanRight' [] = ([], Nothing)
spanRight' ((Right x) : xs) = let (ys, zs) = spanRight' xs
                              in  (x : ys, zs)
spanRight' ((Left x) : xs) = ([], Just (x, xs))

{- |

>>> partition [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
([1,2,3],"abc")

-}
partition :: [Either a b] -> ([a], [b])
partition [] = ([], [])
partition (x : xs) = let (as, bs) = partition xs
                     in  case x of Left a  -> (a : as, bs)
                                   Right b -> (as, b : bs)
