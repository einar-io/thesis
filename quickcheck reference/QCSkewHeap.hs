
-- Roughly based on code from the paper
-- "QuickCheck Testing for Fun and Profit" by John Hughes

import Test.QuickCheck

import qualified SkewHeap as SH
import qualified Data.List as List


------------------------
-- Symbolic expressions
------------------------

data Opr = Insert Integer
         | DeleteMin
         deriving Show

data SymbolicHeap = SymHeap [Opr]
                  deriving Show

eval (SymHeap ops) = foldl op SH.empty ops
  where op h (Insert n) = SH.insert n h
        op h DeleteMin  = SH.deleteMin h


------------------------
-- Generating symbolic expressions
------------------------

instance Arbitrary Opr where
  arbitrary = frequency [ (2, do n <- arbitrary;
                                 return (Insert n))
                        , (1, return DeleteMin)]

instance Arbitrary SymbolicHeap where
  arbitrary = fmap SymHeap arbitrary
  shrink (SymHeap oprs) = map SymHeap (shrink oprs)

------------------------
-- Making a model
------------------------

model :: SH.Tree Integer -> [Integer]
model h = List.sort $ SH.toList h  -- try to remove the sorting

(f `models` g) h =
  f (model h) === model (g h)


------------------------
-- Commuting diagrams
------------------------

prop_insert n symHeap =
  ((List.insert n) `models` SH.insert n) h
  where h = eval symHeap

prop_deleteMin symHeap =
  SH.size h>0 ==> (tail `models` SH.deleteMin) h
  where h = eval symHeap
