{-# LANGUAGE ExtendedDefaultRules #-}

module Plot
  ( -- savePlot
  plotMeasurements
  ) where

import Types
import Graphics.Matplotlib
import Flow

{-
avg :: Series -> Double
avg x = sum x / fromIntegral (length x)

testAvg :: Series
testAvg = map avg [[1,2,3,4],[2,3,6,4,9,29444431]]

savePlot :: PlotData -> IO (Either String String)
savePlot (name, oom, seriess) =
    -- | Based on http://matplotlib.org/examples/pylab_examples/legend_demo3.html
    -- start stop steps
    --let avgs = map avg seriess
    let avgs = map minimum seriess
        fig = line oom avgs @@ [o2 "label" "Symbolic"]
            % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Implementations", o2 "loc" "upper left"]
            % title ("Performance characteristics for " ++ name ++ " (log-log)")
            % xlabel "Length of input vector"
            % ylabel "Time in µs"
            % grid True
            % mp # "ax.set_xscale('log', base=2)"
            % mp # "ax.set_yscale('log', base=2)"
            -- % mp # "ax.set_xlim(left=512)" -- 2^9
            -- % mp # "ax.set_ylim(bottom=0)" -- does not work for logarithmic scales
        filepath = "build/" ++ name ++ ".svg"
     in file filepath fig
            {-
           print "DEBUG"
           print "is:"
           print $ "len(is):" ++ show (length oom)
           print oom
           print "seriess:"
           print $ "len(seriess):" ++ show (length seriess)
           print seriess
           print "avgs:"
           print $ "len(avgs):" ++ show (length avgs)
           print avgs
           -}
-}


plotMeasurements :: String -> [PlotData] -> IO (Either String String)
plotMeasurements plottitle pds =
  let makeLine (seriesname, color, (xs, ys)) = line xs ys @@ [o2 "label" seriesname, o2 "color" color]
      lines = map makeLine pds
      fig = mp
          % foldl (%) mp lines
          % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Implementations", o2 "loc" "upper left"]
          % xlabel "Length of input vector"
          % title ("Performance characteristics for " ++ plottitle ++ " (Log-Log)")
          % ylabel "Time in µs"
          % grid True
          % mp # "ax.set_xscale('log', base=2)"
          % mp # "ax.set_yscale('log', base=2)"
          -- % mp # "ax.set_xlim(left=512)" -- 2^9
          -- % mp # "ax.set_ylim(bottom=0)" -- does not work for logarithmic scales
      filepath = "build/" ++ plottitle ++ ".svg"
   in file filepath fig
