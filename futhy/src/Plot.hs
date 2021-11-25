{-# LANGUAGE ExtendedDefaultRules #-}

module Plot
  ( --main
    savePlot
  ) where

import Types
import Graphics.Matplotlib

{-
main :: IO ()
main = do
--  json <- readFile "build/einartest.json"
  -- let series  = json2series json
  let seriess = [[1.0, 2.0, 4.0], [4.0, 7.0, 9.0]] :: [Series]
  _ <- savePlot ("myplot", seriess)
  return ()
-}

savePlot :: (FilePath, [Series]) -> IO FilePath
savePlot (filename, series) =
    -- | Based on http://matplotlib.org/examples/pylab_examples/legend_demo3.html
    -- start stop steps
    let fig = plotMapLinear (\x -> x ** 2) 0 1 100 @@ [o2 "label" "Ulrik"]
            % plotMapLinear (\x -> x ** 3) 0 1 100 @@ [o2 "label" "Einar"]
            % line (series !! 0) (series !! 1) @@ [o2 "label" "en benchmark serie"]
            % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]
        filepath = "build/" ++ filename ++ ".svg"
    in getLine >> file filepath fig >>= \_ -> return filepath
