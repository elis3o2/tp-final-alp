module Plot where

import Data.GraphViz
import Common
import Distribution
import Graphics.EasyPlot
import qualified Data.Vector as V

plotRand :: RandVar -> IO ()
plotRand (Disc x) = plotDisc x 
plotRand (Cont x) = plotCont x 


plotDisc :: VarDisc -> IO ()
plotDisc x = do
  let pts = buildProbDisc x
      n   = nameDisc x
  _ <- plot (PNG (n ++ ".png"))
        [ Data2D [Title "", Graphics.EasyPlot.Style Points] [] pts
        , Data2D [Title "",  Graphics.EasyPlot.Style Lines]  [] pts
        ]
  return ()

  
plotCont :: VarCont -> IO ()
plotCont x = do 
  let n = nameCont x
      m = getMeanCont x
      s = sqrt (getVarianceCont x)
      a = m - 4 * s
      b = m + 4 * s
      step = (b - a) / 200  
      xs = [a, a + step .. b]
      pts = [(t, functionCont x t) | t <- xs]
  _ <- plot (PNG (n ++ ".png"))
        [ Data2D [Title "", Graphics.EasyPlot.Style Lines] [] pts ]

  return ()







params :: GraphvizParams Int String String () String
params = nonClusteredParams
  { fmtNode = \(_, l) -> [toLabel l]
  , fmtEdge = \(_, _, l) -> [toLabel l]
  }


makeNodes :: Common.Path -> [(Int, String)]
makeNodes names = zip [0..] (V.toList names)

makeEdges :: Matrix Double -> [(Int, Int, String)]
makeEdges matrix = [ (i, j, show p) | (i, row) <- zip [0..] (V.toList matrix)
                                    , (j, p)   <- zip [0..] (V.toList row)
                                    , p > 0 ]

markovToGraph :: Markov -> DotGraph Int
markovToGraph (Mk names matrix) = 
    graphElemsToDot params (makeNodes names) (makeEdges matrix)


plotMarkov :: Markov ->  IO ()
plotMarkov mk = do _ <- runGraphviz (markovToGraph mk) Png "markov.png"
                   return ()