{-# LANGUAGE ExtendedDefaultRules #-}

module Plot
  ( main
  , writePlot
  ) where

import Graphics.Matplotlib

  --main :: IO ()
main = do
  let series = [[1.0, 2.0, 4.0], [4.0, 7.0, 9.0]]
  writePlot series "myplot"

--writePlot :: [[Int]] -> FilePath -> IO FilePath
writePlot series filename =
    -- | Based on http://matplotlib.org/examples/pylab_examples/legend_demo3.html
    let plot = plotMapLinear (\x -> x ** 2) 0 1 100 @@ [o2 "label" "Ulrik"]
             % plotMapLinear (\x -> x ** 3) 0 1 100 @@ [o2 "label" "Einar"]
             % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]
        fp = "build/" ++ filename ++ ".svg"
    in file fp plot
