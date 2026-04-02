module Table where

import AST
import Common
import Eval
import Monad
import Lib
import Global
import Data.List (transpose)
import qualified Text.PrettyPrint.Boxes as B
import Numeric (showFFloat)
import qualified Data.Vector as V
import Error
import Validator
import Graphics.EasyPlot


formatDouble :: Double -> String
formatDouble x = showFFloat (Just 24) x ""


renderTable :: [String] -> [[String]] -> String
renderTable headers rows =
  B.render $
    B.vsep 0 B.left $
      [ sepLine
      , rowHeader headers
      , sepLine
      ]
      ++ concatMap (\r -> [row r, sepLine]) rows
  where
    -- ancho de cada columna
    colWidths =
      map (maximum . map length) $
        transpose (headers : rows)

    -- alinear derecha (para números)
    padLeft w s = replicate (w - length s) ' ' ++ s

    -- centrar (para headers)
    padCenter w s =
      let total = w - length s
          left  = total `div` 2
          right = total - left
      in replicate left ' ' ++ s ++ replicate right ' '

    -- fila normal
    row xs =
      B.hcat B.left $
        [B.text "|"]
        ++ concat
            [ [ B.text (" " ++ padLeft w x ++ " ")
              , B.text "|"
              ]
            | (w, x) <- zip colWidths xs
            ]

    -- header centrado
    rowHeader xs =
      B.hcat B.left$
        [B.text "|"]
        ++ concat
            [ [ B.text (" " ++ padCenter w x ++ " ")
              , B.text "|"
              ]
            | (w, x) <- zip colWidths xs
            ]

    -- línea separadora
    sepLine =
      B.text $
        concatMap (\w -> "+" ++ replicate (w + 2) '-') colWidths ++ "+"


buildProbs :: VarDisc -> [[String]]
buildProbs x@(Bin n _)     = [[show k, formatDouble (getProbDisc x Eq k)] | k <- [0..n]     ]
buildProbs x@(Poiss _)     = [[show k, formatDouble (getProbDisc x Eq k)] | k <- [0..10]    ]
buildProbs x@(Geo _)       = [[show k, formatDouble (getProbDisc x Eq k)] | k <- [0..10]    ]
buildProbs x@(Pasc r _)    = [[show k, formatDouble (getProbDisc x Eq k)] | k <- [0..r]     ]
buildProbs x@(Hiper m _ _) = [[show k, formatDouble (getProbDisc x Eq k)] | k <- [0..m]     ]
buildProbs x@(Custom xs _) = [[show k, formatDouble (getProbDisc x Eq k)] | k <- V.toList xs]


buildProbsR :: MonadProb m => RandVar -> Int -> Int -> m [[String]]
buildProbsR x@(Disc (Custom xs _)) i f | i > f = throwErrorE ProbInvalidForm
                                       | otherwise = mapM buildRow list 
                                          where
                                            list = V.toList (V.filter (\a-> a < i || a > f) xs)
                                            buildRow k = do
                                              p <- getProb x Eq k
                                              return [show k, formatDouble p]
buildProbsR x@(Disc _) i f | i > f = throwErrorE ProbInvalidForm
                           | otherwise = mapM buildRow [i .. f]
                             where
                                buildRow k = do
                                  p <- getProb x Eq k
                                  return [show k, formatDouble p]
buildProbsR _  _ _= throwErrorE InvalidProb




makeTableR :: MonadProb m => Value -> Value -> Value -> m String
makeTableR (VRand v) (VNum x) (VNum y) = do x' <- toInt x
                                            y' <- toInt y
                                            probs <- buildProbsR v x' y'
                                            return (renderTable  ["X", "P(X)"] probs)
makeTableR _ _ _ = throwErrorE InvalidProb


makeTable :: MonadProb m => Value -> m String 
makeTable (VRand (Disc v)) = return (renderTable ["X", "P(X)"] (buildProbs v))
makeTable _ = throwErrorE InvalidProb 




getFunctions :: VarCont -> Double -> Double 
getFunctions (Norm m s) x = 1 / (s * sqrt (2 * pi)) *  exp (- ((x - m)^2) / (2 * s^2))
getFunctions (Expo l) x | x < 0  = 0
                        | otherwise  = l * exp (-l * x)
getFunctions (Unif a b) x | x < a || x > b = 0
                          | otherwise = 1 / (b - a)


getScale :: VarDisc -> [(Double, Double)]
getScale x@(Bin n _)     = [(fromIntegral k, getProbDisc x Eq k) | k <- [0..n]     ]
getScale x@(Poiss _)     = [(fromIntegral k, getProbDisc x Eq k) | k <- [0..10]    ]
getScale x@(Geo _)       = [(fromIntegral k, getProbDisc x Eq k) | k <- [0..10]    ]
getScale x@(Pasc r _)    = [(fromIntegral k, getProbDisc x Eq k) | k <- [0..r]     ]
getScale x@(Hiper m _ _) = [(fromIntegral k, getProbDisc x Eq k) | k <- [0..m]     ]
getScale x@(Custom xs _) = [(fromIntegral k, getProbDisc x Eq k) | k <- V.toList xs]

name :: VarDisc -> String
name (Bin _ _)   = "binomial"
name (Poiss _)   = "poisson"
name (Geo _)     = "geometrica"
name (Pasc _ _)  = "pascal"
name (Hiper _ _ _) = "hipergeometrica"
name (Custom _ _)  = "custom"

plotDisc :: VarDisc -> IO ()
plotDisc x = do
  let pts = getScale x
      n   = name x
  _ <- plot (PNG (n ++ ".png"))
        [ Data2D [Title (n ++ " (points)"), Style Points] [] pts
        , Data2D [Title (n ++ " (lines)"),  Style Lines]  [] pts
        ]
  return ()


plotCont :: VarCont -> IO ()
plotCont x = do 
  _ <- plot (PNG "normal.png") $ Function2D [] [] (getFunctions x)
  return ()