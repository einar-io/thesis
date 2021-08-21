module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LFun a b
  = Scale Real

 Real -> LFun v v

--int :: LFun a b -> (a -> b)
--comp :: LFun a b -> Futh a b

IMPLEMENT A WAY TO EXPRESS LINEAR MAPS

--data LFun a b where
--Scale :: Real -> LFun V V
--Vectors used from some

--int id x = x
