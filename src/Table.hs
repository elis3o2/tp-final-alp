module Table where

import AST
import Common
import Monad
import qualified Text.PrettyPrint.Boxes as B
import qualified Data.Vector as V
import Error
import Validator
import Distribution
import PPrint
import Global
import Control.Monad.Reader (asks)


renderTable :: String -> [String] -> String
renderTable header rows =
  B.render $
    B.vsep 0 B.left $
      [ sepLine
      , rowHeader header
      , sepLine
      ]
      ++ concatMap (\r -> [row r, sepLine]) rows
  where
    colWidth :: Int
    colWidth = maximum (length header : map length rows)

    padRight :: Int -> String -> String
    padRight w s = s ++ replicate (w - length s) ' '

    padCenter :: Int -> String -> String
    padCenter w s =
      let total = w - length s
          left  = total `div` 2
          right = total - left
      in replicate left ' ' ++ s ++ replicate right ' '

    row :: String -> B.Box
    row x =
      B.hcat B.left
        [ B.text "|"
        , B.text (" " ++ padRight colWidth x ++ " ")
        , B.text "|"
        ]

    rowHeader :: String -> B.Box
    rowHeader x =
      B.hcat B.left
        [ B.text "|"
        , B.text (" " ++ padCenter colWidth x ++ " ")
        , B.text "|"
        ]

    sepLine :: B.Box
    sepLine =
      B.text $
        "+" ++ replicate (colWidth + 2) '-' ++ "+"


formatProbRow :: PPConf -> Double -> Double -> String
formatProbRow conf k p = formatDouble conf k ++ " | " ++ formatDouble conf p


buildProbR :: MonadProb m => RandVar -> Int -> Int -> m [String]
buildProbR (Disc v@(Custom xs _)) i f | i > f     = throwErrorE ProbInvalidForm
                                      | otherwise = do
                                          d <- asks decimals
                                          let ppconf = PPConf d
                                              values = filter (\k -> k >= i && k <= f) (V.toList xs)
                                          mapM (buildRow ppconf) values
                                      where
                                        buildRow ppconf k =
                                          return $ formatProbRow ppconf (fromIntegral k) (functionDisc v k)
buildProbR (Disc v) i f | i > f     = throwErrorE ProbInvalidForm
                        | otherwise = do 
                                d <- asks decimals
                                let ppconf = PPConf d 
                                mapM (buildRow ppconf) [i .. f]
                      where
                        buildRow ppconf k = do
                          return (formatProbRow ppconf (fromIntegral k) (functionDisc v k))

buildProbR _ _ _ = throwErrorE InvalidProb


makeTableR :: MonadProb m => Value -> Value -> Value -> m String
makeTableR (VRand (Disc v)) (VNum x) (VNum y) = do
  x' <- toInt x
  y' <- toInt y
  probs <- buildProbR (Disc v) x' y'
  return (renderTable (nameDisc v) probs)
makeTableR _ _ _ = throwErrorE InvalidProb


makeTable :: MonadProb m => Value -> m String
makeTable (VRand (Disc v)) = do 
  d <- asks decimals
  let ppconf = PPConf d
      rows = map (uncurry (formatProbRow ppconf)) (buildProbDisc v)
  return (renderTable (nameDisc v) rows)

makeTable _ = throwErrorE InvalidProb