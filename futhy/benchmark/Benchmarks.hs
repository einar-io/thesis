--module Benchmarks (main) where

import Test.Tasty.Bench

countDown :: Int -> Int
countDown a
  | a == 0    = 1 -- print "bottom"
  | otherwise = countDown (a-1)

main :: IO ()
main = defaultMain
  [ bgroup "Counting Down"
    [ bench "1000" $ nf countDown 1000
    , bench "10000" $ nf countDown 10000
    , bench "100000" $ nf countDown 100000
    ]
  ]


