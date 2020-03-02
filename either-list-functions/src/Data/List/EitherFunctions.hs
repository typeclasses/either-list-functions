{-# LANGUAGE BlockArguments, LambdaCase, NoImplicitPrelude #-}

-- | Functions involving lists of 'Either'.

module Data.List.EitherFunctions
  (

    {- * Map       -}  partlyMap,
    {- * Group     -}  groupEither,
    {- * Partition -}  partition,
    {- * Span      -}  spanLeft, spanLeft', spanRight, spanRight',
    {- * Lead      -}  leadLeft, leadLeft', leadRight, leadRight',
    {- * Branch    -}  branchLeft, branchRight, BranchComparison

  ) where

import Data.Bool                   ( Bool (..) )
import Data.Either                 ( Either (..) )
import Data.Function               ( fix )
import Data.Functor.Contravariant  ( Comparison (..), contramap )
import Data.List                   ( foldr, map, span )
import Data.Tree                   ( Tree (..), Forest )
import Data.Maybe                  ( Maybe (..), maybe )
import Data.Ord                    ( Ordering (..) )

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
-- >>> leadLeft [Right 'a', Right 'b', Left 1, Right 'c', Right 'd', Left 2, Right 'e', Right 'f']
-- ("ab",[(1,"cd"),(2,"ef")])
--
-- >>> leadLeft [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- ("",[(1,""),(2,"a"),(3,"bc")])

leadLeft :: [Either a b] -> ([b], [(a, [b])])

leadLeft = f
  where
    f xs = (unledItems, ledGroups)
      where
        (unledItems, ysMaybe) = spanRight' xs
        ledGroups = case ysMaybe of
            Nothing            ->  []
            Just (leader, ys)  ->  r leader ys

    r leader xs = firstGroup : moreGroups
      where
        firstGroup = (leader, followers)
        (followers, ysMaybe) = spanRight' xs
        moreGroups = case ysMaybe of
            Nothing             ->  []
            Just (leader', ys)  ->  r leader' ys

-- |
-- >>> leadLeft' 0 [Right 'a', Right 'b', Left 1, Right 'c', Right 'd', Left 2, Right 'e', Right 'f']
-- [(0,"ab"),(1,"cd"),(2,"ef")]
--
-- >>> leadLeft' 0 [Left 1, Left 2, Right 'a', Left 3, Right 'b', Right 'c']
-- [(1,""),(2,"a"),(3,"bc")]

leadLeft' ::
    a -- ^ Leader to use for the first group in case the list does not begin with a 'Left'.
    -> [Either a b] -> [(a, [b])]

leadLeft' leader xs = addMissingLeader leader (leadLeft xs)

-- |
-- >>> leadRight [Left 1, Left 2, Right 'a', Left 3, Left 4, Right 'b', Left 5, Left 6]
-- ([1,2],[('a',[3,4]),('b',[5,6])])
--
-- >>> leadRight [Right 'a', Left 3, Left 4, Right 'b', Right 'c', Left 5, Left 6]
-- ([],[('a',[3,4]),('b',[]),('c',[5,6])])

leadRight :: [Either a b] -> ([a], [(b, [a])])

leadRight = f
  where
    f xs = (unledItems, ledGroups)
      where
        (unledItems, ysMaybe) = spanLeft' xs
        ledGroups = case ysMaybe of
            Nothing            ->  []
            Just (leader, ys)  ->  r leader ys

    r leader xs = firstGroup : moreGroups
      where
        firstGroup = (leader, followers)
        (followers, ysMaybe) = spanLeft' xs
        moreGroups = case ysMaybe of
            Nothing             ->  []
            Just (leader', ys)  ->  r leader' ys

-- |
-- >>> leadRight' 'z' [Left 1, Left 2, Right 'a', Left 3, Left 4, Right 'b', Left 5, Left 6]
-- [('z',[1,2]),('a',[3,4]),('b',[5,6])]
--
-- >>> leadRight' 'z' [Right 'a', Left 3, Left 4, Right 'b', Right 'c', Left 5, Left 6]
-- [('a',[3,4]),('b',[]),('c',[5,6])]

leadRight' ::
    b -- ^ Leader to use for the first group in case the list does not begin with a 'Right'.
    -> [Either a b] -> [(b, [a])]

leadRight' leader xs = addMissingLeader leader (leadRight xs)

addMissingLeader :: a -> ([b], [(a, [b])]) -> [(a, [b])]

addMissingLeader _      ( []         , groups ) =                        groups
addMissingLeader leader ( unledIntro , groups ) = (leader, unledIntro) : groups

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
    []            ->  ( []     , []     )
    Left  a : xs  ->  ( a : as , bs     )  where (as, bs) = r xs
    Right b : xs  ->  ( as     , b : bs )  where (as, bs) = r xs

-- | The relative significance of branches (greater values are closer to the root).
type BranchComparison a = Comparison a

-- |
-- >>> import Prelude
--
-- >>> heading level title = Left (level, title)
-- >>> chapter = heading 1
-- >>> section = heading 2
-- >>> p text = Right text
--
-- >>> :{
-- >>> list =
-- >>>     [ p "Copyright"
-- >>>     , p "Preface"
-- >>>     , chapter "Animals"
-- >>>     , p "The kingdom animalia"
-- >>>     , section "Vertibrates"
-- >>>     , p "Cats"
-- >>>     , p "Snakes"
-- >>>     , section "Invertibrates"
-- >>>     , p "Worms"
-- >>>     , p "Jellyfishes"
-- >>>     , chapter "Fungus"
-- >>>     , p "Yeast"
-- >>>     , p "Truffles"
-- >>>     , p "Morels"
-- >>>     ]
-- >>> :}
--
-- >>> import Data.Functor.Contravariant
-- >>> flipComparison (Comparison f) = Comparison (flip f)
-- >>> headingComparison = contramap fst (flipComparison defaultComparison)
--
-- >>> (frontMatter, mainMatter) = branchLeft headingComparison list
--
-- >>> frontMatter
-- ["Copyright","Preface"]
--
-- >>> import Data.List
-- >>> showContent ((_, x), ys) = x ++ ": " ++ intercalate ", " ys
--
-- >>> import Data.Tree
-- >>> putStrLn $ drawForest $ map (fmap showContent) mainMatter
-- Animals: The kingdom animalia
-- |
-- +- Vertibrates: Cats, Snakes
-- |
-- `- Invertibrates: Worms, Jellyfishes
-- <BLANKLINE>
-- Fungus: Yeast, Truffles, Morels
-- <BLANKLINE>
-- <BLANKLINE>

branchLeft :: BranchComparison a -> [Either a b] -> ([b], Forest (a, [b]))

branchLeft c xs = (rejects, forest)
  where
    (rejects, nodes) = leadLeft xs
    forest = makeForest c' nodes
    c' = contramap (\(x, _) -> x) c

-- | Same as 'branchLeft', but with the types flipped; here, 'Right' is the case that indicates a branch.

branchRight :: BranchComparison b -> [Either a b] -> ([a], Forest (b, [a]))

branchRight c xs = (rejects, forest)
  where
    (rejects, nodes) = leadRight xs
    forest = makeForest c' nodes
    c' = contramap (\(x, _) -> x) c

makeForest :: BranchComparison a -> [a] -> Forest a

makeForest c = foldr f []
  where
    f x xs = Node x chomped : remainder
      where
        (chomped, remainder) = span (\(Node y _) -> x > y) xs

    x > y = case getComparison c x y of GT -> True; _ -> False
