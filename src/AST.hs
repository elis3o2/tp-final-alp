module AST where

import qualified Data.Vector as V

type Name = String


-- Operadores
data OpComp  = Lt | Gt | Lte | Gte | Eq | NEq deriving (Show, Eq)

data OpBin = Plus | Minus | Times | Div deriving (Show, Eq)

type Vec a = V.Vector a

-- valores
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

data VarAle = Disc VarDisc | Cont VarCont   deriving (Show, Eq)


-- expresiones
data ExpDisc =
    BinE ExpNum ExpNum
  | PoissE ExpNum
  | GeoE ExpNum
  | PascE ExpNum ExpNum
  | HiperE ExpNum ExpNum ExpNum
  | CustomE ExpVec ExpVec

  deriving (Show, Eq)

data ExpCont = NormE ExpNum ExpNum
             | UnifE ExpNum ExpNum
             | ExpoE ExpNum

  deriving (Show, Eq)

data ExpAle = VarA  Name
            | DiscE ExpDisc
            | ContE ExpCont

    deriving (Show, Eq)




data NumC = I Int | D Double deriving (Show, Eq)


data ExpNum
  = ConstN NumC 
  | VarN Name
  | UMinus ExpNum
  | OpNum OpBin ExpNum ExpNum
  
  -- acceso a vector
  | Access ExpVec ExpNum

  -- funciones sobre variables aleatorias
  | Esp ExpAle
  | Vari ExpAle
  | Desv ExpAle
  | FDP ExpAle ExpNum
  | MaxP ExpAle
  | MaxFDP ExpAle

  -- probabilidades
  | POp ExpAle OpComp ExpNum
  | POpBt ExpAle OpComp ExpNum OpComp ExpNum

  deriving (Show, Eq)



data ExpVec = 
    VarV Name 
  | ConstV (Vec ExpNum)
  -- Funciones sobre variables aleatorias
  | Moda  ExpAle
  deriving (Show, Eq)


data Exp =
    ENum ExpNum
  | EVec ExpVec
  | EAle ExpAle


data Value
  = VNum NumC
  | VVec (Vec NumC)
  | VAle VarAle


data Decl a = Decl {
  declName :: Name,
  declBody :: a
} 

data Type = TNum | TVec | TAle

data Comm = Print Exp | Let Name Exp