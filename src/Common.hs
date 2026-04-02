module Common where 
import qualified Data.Vector as V





type Vec a = V.Vector a
type Matrix a = Vec (Vec a)
type Name = String
type Path = Vec String

-- Operadores
data OpComp  = Lt | Gt | Lte | Gte | Eq | NEq deriving (Show, Eq)

data OpBin = Plus | Minus | Times | Div deriving (Show, Eq)


indexOf :: Eq a => Vec a -> a -> Int
indexOf xs k =
  case V.elemIndex k xs of
    Just i  -> i
    Nothing -> error "Invalid Index"


