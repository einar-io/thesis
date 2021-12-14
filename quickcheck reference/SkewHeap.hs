-- Roughly based on code from the paper
-- "QuickCheck Testing for Fun and Profit" by John Hughes

module SkewHeap
  ( Tree(..)
  , empty
  , minElem
  , insert
  , deleteMin
  , toList
  , fromList
  , size
  )
where

data Tree a = Null | Fork a (Tree a) (Tree a)
            deriving (Eq, Show)

empty = Null

-- | the value in each node is less than any value in its subtrees
invariant :: Ord a => Tree a -> Bool
invariant Null = True
invariant (Fork x left right) = smaller x left && smaller x right

smaller x Null = True
smaller x t@(Fork y _ _) = x <= y && invariant t

-- | extract the minimum element
minElem (Fork x _ _) = x

-- | insert element into the heap
-- allways insert into left subtree, but then swap the subtrees
insert :: Ord a => a -> Tree a -> Tree a
insert x Null = Fork x Null Null
insert x (Fork y left right) = Fork (min x y) right (insert (max x y) left)

deleteMin Null                = Null
deleteMin (Fork x left right) = merge left right


merge t Null = t
merge Null t = t
merge left right | minElem left <= minElem right = join left right
                 | otherwise                     = join right left
  where join (Fork x left right) h = Fork x right (merge left h)


balanced :: Tree a -> Bool
balanced Null = True
balanced (Fork _ left right) = (d==0 || d==1) && balanced left && balanced right
  where d = weight right - weight left

weight Null         = 0
weight (Fork _ l r) = 1 + weight l + weight r

fromList :: Ord a => [a] -> Tree a
fromList ns = foldl (\h n -> insert n h) empty ns

toList :: Tree a -> [a]
toList Null = []
toList (Fork a l r) = a : toList l ++ toList r

size :: Tree a -> Integer
size Null = 0
size (Fork a l r) = 1 + size l + size r
