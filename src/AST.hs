{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module AST where

import Common
import qualified Data.Vector as V

-- =========================
-- Variables aleatorias
-- =========================

data VarDisc
  = Bin Int Double
  | Poiss Double
  | Geo Double
  | Pasc Int Double
  | Hiper Int Int Int
  | Custom (Vec Int) (Vec Double)
  deriving (Show, Eq)

data VarCont
  = Norm Double Double
  | Unif Double Double
  | Expo Double
  deriving (Show, Eq)


-- Expresiones para variables aleatorias
data ExpDisc
  = BinE Exp Exp
  | PoissE Exp
  | GeoE Exp
  | PascE Exp Exp
  | HiperE Exp Exp Exp
  | CustomE Exp Exp
  deriving (Show, Eq)

data ExpCont
  = NormE Exp Exp
  | UnifE Exp Exp
  | ExpoE Exp
  deriving (Show, Eq)

data RandExp = ContE ExpCont | DiscE ExpDisc deriving (Show, Eq)

data RandVar = Cont VarCont | Disc VarDisc deriving (Show, Eq)


-- =========================
-- Markov
-- =========================

data NodeVal = NodeVal [(Name, Double)] deriving (Show, Eq)

data NodeExp = NodeExp [(Name, Exp)] deriving (Show, Eq)

data Markov = MarkovChain (Vec Name) (Matrix Double) deriving (Show, Eq)

data MarkovExp
  = MkMarkov Chain
  | Stationary Exp
  | NextDist Exp Exp


  | SimulFromName Exp Name Exp
  | SimulFromVec Exp Exp Exp
  deriving (Show, Eq)


-- =========================
-- Numérico / estadística / probabilidad
-- =========================

data NumExp
  = ConstN NumC
  | UMinus Exp
  | OpNum OpBin Exp Exp
  deriving (Show, Eq)

data StatExp
  = Mean Exp
  | Variance Exp
  | StdDev Exp
  | FDP Exp Exp
  | MaxP Exp
  | MaxFDP Exp
  | Mode Exp
  deriving (Show, Eq)

data ProbExp
  = Prob Exp OpComp Exp
  | ProbBetween Exp OpComp Exp OpComp Exp
  
  | StepProb Exp Name Name Exp
  | PathProb Exp Chain
  | HitProb Exp Name Name

  deriving (Show, Eq)


-- =========================
-- Vectores
-- =========================

data VecExp = ConstV (Vec Exp) | Access Exp Exp  deriving (Show, Eq)

data ChainExp = ConstCh Chain
-- =========================
-- Expresión principal
-- =========================

data Exp
  = VarRef Name
  | NumE NumExp
  | VecE VecExp
  | RandE RandExp
  | StatE StatExp
  | ProbE ProbExp
  | MarkovE MarkovExp
  | NodeE NodeExp
  | ChainE ChainExp
  deriving (Show, Eq)


-- =========================
-- Valores
-- =========================

data Value
  = VNum NumC
  | VVec (Vec NumC)
  | VRand RandVar
  | VMarkov Markov
  | VNode NodeVal
  | VChain Chain
  deriving (Show, Eq)


-- =========================
-- Comandos
-- =========================

data Comm
  = Print Exp
  | Let Name Exp
  | Table Exp
  | TableR Exp Exp Exp
  | Plot Exp
  deriving (Show, Eq)