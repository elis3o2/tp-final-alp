{-|
Module      : AST
Description : Language Definition
-}
module AST where
import Common


-- expressions
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


newtype NodeExp = NE [(Name, Exp)]    deriving (Show, Eq)


newtype MarkovExp = MarkovE Path    deriving (Show, Eq)


data Exp
  = VarRef Name          -- Double | Vec Double | RandVar | Markov
  | ConstN Double        -- Double
  | UMinus Exp           -- NumExp -> Double
  | OpNum OpBin Exp Exp  -- NumExp -> NumExp -> Double 


  -- Stats 
  | Mean Exp     -- RandExp -> Double
  | Variance Exp -- RandExp -> Double
  | StdDev Exp   -- RandExp -> Double
  | Mode  Exp    -- RandExp -> Vec (Double)
  | PDF Exp Exp  -- ContExp -> NumExp -> Double
  | MaxP Exp     -- DiscExp -> Double
  | MaxPDF Exp   -- DiscExp -> Double

  -- probabilidades
  | Prob Exp OpComp Exp                    -- RandExp -> OpComp -> NumExp --> Double
  | ProbBetween Exp OpComp Exp OpComp Exp  -- RandExp -> OpComp -> NumExp --> OpComp -> NumExp --> Double

  -- vectoriales
  | ConstV (Vec Exp)  -- Vec NumExp -> Vec Double
  | Access Exp Exp    -- VecExp -> NumExp -> Double

  -- aleatorias 
  | Rand RandExp      --  RandExp  

  -- Cadenas de Markov
  | ConstCh Path                -- Path
  | Markov MarkovExp            -- Markov

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

 deriving (Show, Eq)

data Type = Num | Vec | RandDisc | RandCont | Mark | Path | Nod deriving (Show,Eq)

data Comm = Print Exp
        | Let Name Exp
        | LetN Name NodeExp 
        | Table Exp 
        | TableR Exp Exp Exp 
        | Plot Exp 
  deriving (Show, Eq)