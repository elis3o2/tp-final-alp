{-|
Module      : Common
Description : Generic definitions
-}
module Common where 
import qualified Data.Vector as V


type Vec a = V.Vector a

indexOf :: Eq a => Vec a -> a -> Int
indexOf xs k =
  case V.elemIndex k xs of
    Just i  -> i
    Nothing -> error "Invalid Index"



type Matrix a = Vec (Vec a)


type Name = String

type Path = Vec Name

-- | Operators
data OpComp  = Lt | Gt | Lte | Gte | Eq | NEq deriving (Show, Eq)

data OpBin = Plus | Minus | Times | Div deriving (Show, Eq)

-- | Random Variables
data VarDisc =
    Bin Int Double
  | Poiss Double
  | Geo Double
  | Pasc Int Double
  | Hiper Int Int Int
  | Custom (Vec Int) (Vec Double)

  deriving (Show, Eq)

data VarCont = Norm Double Double
             | Unif Double Double
             | Expo Double

  deriving (Show, Eq)

data RandVar = Cont  VarCont | Disc  VarDisc deriving (Show, Eq)



-- | Markov Chains
data Markov = Mk (Vec Name) (Matrix Double)   deriving (Show, Eq)

newtype NodeVal = N [(Name, Double)]    deriving (Show, Eq)

