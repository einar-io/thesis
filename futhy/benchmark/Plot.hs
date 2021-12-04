{-# LANGUAGE ExtendedDefaultRules #-}

module Plot
  ( --main
    savePlot
  ) where

--import Data.List
import Types
import Graphics.Matplotlib
--import Flow
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

avg :: Series -> Double
avg x = sum x / fromIntegral (length x)

testAvg :: Series
testAvg = map avg [[1,2,3,4],[2,3,6,4,9,29444431]]

savePlot :: PlotData -> IO ()
savePlot (name, oom, seriess) =
    -- | Based on http://matplotlib.org/examples/pylab_examples/legend_demo3.html
    -- start stop steps
    --let avgs = map avg seriess
    let avgs = map minimum seriess
        fig = line oom avgs @@ [o2 "label" "Symbolic"]
            % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Implementations", o2 "loc" "upper left"]
            % title ("Performance characteristics for " ++ name)
            % xlabel "Length of input vector"
            % ylabel "Time in µs"
            % grid True
            % mp # "ax.set_xscale('log', base=2)"
            % mp # "ax.set_yscale('log', base=2)"
            -- % mp # "ax.set_xlim(left=512)" -- 2^9
            -- % mp # "ax.set_ylim(bottom=0)" -- does not work for logarithmic scales
        filepath = "build/" ++ name ++ ".svg"
     in do print "DEBUG"
           print "is:"
           print $ "len(is):" ++ show (length oom)
           print oom
           print "seriess:"
           print $ "len(seriess):" ++ show (length seriess)
           print seriess
           print "avgs:"
           print $ "len(avgs):" ++ show (length avgs)
           print avgs
           _ <- file filepath fig
           return ()
