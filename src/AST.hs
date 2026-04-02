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


data NodeVal = N [(Name, Double)]    deriving (Show, Eq)
data NodeExp = NE [(Name, Exp)]    deriving (Show, Eq)

data Markov = Mk (Vec Name) (Matrix Double)   deriving (Show, Eq)

data MarkovExp = MarkovE Path    deriving (Show, Eq)


data Exp
  = VarRef Name          -- Double | Vec Double | RandVar | Markov
  | ConstN Double          -- Double
  | UMinus Exp           -- ExpNumC -> Double
  | OpNum OpBin Exp Exp  -- ExpNumC -> ExpNumC -> Double 


  -- Stats 
  | Mean Exp     -- RandExp -> Double
  | Variance Exp -- RandExp -> Double
  | StdDev Exp   -- RandExp -> Double
  | Mode  Exp    -- RandExp -> Vec (Double)
  | FDP Exp Exp  -- ContExp -> Double
  | MaxP Exp     -- DiscExp -> Double
  | MaxFDP Exp   -- DiscExp -> Double

  -- probabilidades
  | Prob Exp OpComp Exp                    -- RandExp -> OpComp -> NumExp --> Double
  | ProbBetween Exp OpComp Exp OpComp Exp  -- RandExp -> OpComp -> NumExp --> OpComp -> NumExp --> Double

  -- vectoriales
  | ConstV (Vec Exp)  -- Vec NumExp -> Vec Double
  | Access Exp Exp    -- VecExp -> NumExp -> Double

  -- aleatorias 
  | Rand RandExp      --  RandExp  

  -- Cadenas de Markov
  | ConstCh Path 
  | Markov MarkovExp            --  Markov

  | ProbStep Exp Name Name Exp  -- MarkovExp -> Name -> Name -> NumExp -> Double
  | ProbPath Exp Exp            -- MarkovExp -> PathExp -> Double
  | ProbHit Exp Name Name       -- MarkovExp -> Name -> Name -> Double

  | NextDist Exp Exp            -- MarkovExp -> NumExp -> Markov
  | Stationary Exp              -- MarkovExp -> Markov
  | SimulFromName Exp Name Exp  -- MarkovExp -> Name -> Path
  | SimulFromVec Exp Exp Exp    -- MarkovExp -> Vec Double -> Path


    deriving (Show, Eq)


data Value =
    VNum Double
  | VVec   (Vec Double)
  | VRand  RandVar
  | VMark  Markov
  | VNode  NodeVal
  | VPath Path
  | Dummy

instance Show Value where
  show (VNum   n) = show n
  show (VVec   e) = show e
  show (VRand  x) = show x  
  show (VMark  m) = show m
  show (VNode  n) = show n
  show (VPath c) = show c
  show Dummy      = show "Dummy"



data Type = Num | Vec | RandDisc | RandCont | Mark | Path | Nod deriving(Show,Eq)

data Comm = Print Exp
        | Let Name Exp
        | LetN Name NodeExp 
        | Table Exp 
        | TableR Exp Exp Exp 
        | Plot Exp 
  deriving (Show, Eq)