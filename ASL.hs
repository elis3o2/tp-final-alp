module ASL where

import qualified Data.Vector as V

type Name = String


-- Operadores
data OpComp  = Lt | Gt | Lte | Gte | Eq | NEq deriving (Show, Eq)

data OpBin = Plus | Minus | Times | Div


data NumC = D Double | I Int


data VarAle = Disc VarDisc | Cont VarCont

data VarDisc = Bin ExpNum ExpNum
             | Poiss ExpNum
             | Geo ExpNum
             | Pasc ExpNum ExpNum
             | Hiper ExpNum ExpNum ExpNum
             | Custom ExpVec ExpVec

data VarCont = Norm ExpNum ExpNum
              | Unif ExpNum ExpNum
              | Expo ExpNum



data ExpNum
  = Const NumC
  | VNum Name
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

data ExpVec
  = ConstV (V.Vector ExpNum)
  | VVec Name
  -- Funciones sobre variables aleatorias
  | Moda ExpAle
  deriving (Show, Eq)

data ExpAle
  = VAle Name
  | Const VarAle

  deriving (Show, Eq)


data Decl a = Decl {
  declName :: Name,
  declBody :: a,
}