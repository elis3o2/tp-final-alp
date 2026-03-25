module AST where
import Common


type Name = String

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
  deriving (Show, Eq)

data Value =
    VNum NumC
  | VVec (Vec NumC)
  | VAle VarAle 



instance Show Value where
  show (VNum n) = show n
  show (VVec e) = show e
  show (VAle x) = show x  


data Comm = Print Exp | Let Name Exp