{-# LANGUAGE ExtendedDefaultRules #-}

module Plot
  ( --main
    savePlot
  ) where

import Data.List
import Types
import Graphics.Matplotlib
import Flow
--import Math.Statistics

{-
main :: IO ()
main = do
--  json <- readFile "build/einartest.json"
  -- let series  = json2series json
  let seriess = [[1.0, 2.0, 4.0], [4.0, 7.0, 9.0]] :: [Series]
  _ <- savePlot ("myplot", seriess)
  return ()
-}

avg :: [Series] -> Series
avg = map (\x -> sum x / fromIntegral (length x))

testAvg :: Series
testAvg = avg [[1,2,3,4],[2,3,6,4,9,29444431]]

savePlot :: PlotData -> IO ()
savePlot (name, is, seriess) =
    -- | Based on http://matplotlib.org/examples/pylab_examples/legend_demo3.html
    -- start stop steps
    let avgs = avg seriess
    --let avgs = map minimum seriess
        {-
        fig  = plotMapLinear (\x -> x ** 2) 0 1 100 @@ [o2 "label" "Ulrik"]
             {- % plotMapLinear (\x -> x ** 3) 0 1 100 @@ [o2 "label" "Einar"] -}
             % line (is !! 1) (avgs !! 1)     @@ [o2 "label" name]
             % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper right"]
        -}
        fig = line is avgs  @@ [o2 "label" "Symbolic"]
            % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Implementations", o2 "loc" "upper left"]
            {-% logX
            % logY-}
            % grid True
            {-
            % mp # "ax.set_ylim(bottom=0)"
            % mp # "ax.set_xlim(left=128)"
            -}
            % mp # "ax.set_xscale('log', base=2)"
            % mp # "ax.set_yscale('log', base=2)"
            % ylabel "Time in µs"
            % xlabel "Length of input vector"
            % title ("Performance characteristics for " ++ name)
        filepath = "build/" ++ name ++ ".svg"
     in do print "DEBUG"
           print is
           print seriess
           print avgs
           _ <- file filepath fig
           return ()

{-
genLine :: String -> Int -> Series -> a
genLine name i a = line i a @@ [o2 "label" name]
-}
