{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module AST where
import Common


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


-- expresiones
data ExpDisc =
    BinE Exp Exp
  | PoissE Exp
  | GeoE Exp
  | PascE Exp Exp
  | HiperE Exp Exp Exp
  | CustomE Exp Exp

  deriving (Show, Eq)

data ExpCont = NormE Exp Exp
             | UnifE Exp Exp
             | ExpoE Exp

  deriving (Show, Eq)


data RandExp = ContE ExpCont | DiscE ExpDisc deriving (Show, Eq)
data RandVar = Cont  VarCont | Disc  VarDisc deriving (Show, Eq)



data MakovExp
  = Mk Chain
  | StepProb MarkovExp Exp 
  | Stationary MarkovExp



data Exp
  = VarRef Name
  | ConstN NumC   
  | UMinus Exp
  | OpNum OpBin Exp Exp
  -- acceso a vector
  | Access Exp Exp

  -- funciones sobre variables aleatorias
  | Mean Exp
  | Variance Exp
  | StdDev Exp
  | FDP Exp Exp
  | MaxP Exp
  | MaxFDP Exp

  -- probabilidades
  | Prob Exp OpComp Exp
  | ProbBetween Exp OpComp Exp OpComp Exp

  | Mk Exp Chain
  | MkP  Exp Name Name Exp
  | MakeChValue Exp Exp Exp
  | PMkCh Exp Chain  

  -- vectoriales
  | ConstV (Vec Exp)
  | Mode  Exp

  -- aleatorias 
  | Rand RandExp

  -- Cadenas de Markov
  | Mk MarkovExp


    deriving (Show, Eq)



data Node = N  [(Name, Double)]       deriving (Show, Eq)
data Markov = Mk (Vec Name) (Matrix Double)     deriving (Show, Eq)



data Value =
    VNum NumC
  | VVec (Vec NumC)
  | VRand RandVar
  | VMk Markov
  | VN  Node




instance Show Value where
  show (VNum n) = show n
  show (VVec e) = show e
  show (VAle x) = show x  
  show (VMk  m) = show m
  show (VN   n) = show n


data Comm = Print Exp | Let Name Exp | Table Exp | TableR Exp Exp Exp | Plot Exp 