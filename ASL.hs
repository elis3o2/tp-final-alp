modeule ASL 

type Name = String


data VarDisc = Bin  Expr Expr     |
               Poiss Expr         |
               Geo  Expr         |
               Pasc Expr Expr    |
               Hiper Expr Expr Expr 
            -- T Seq 

data VarCont  = Norm Expr Expr |
                Unif Expr Expr |
                Exp Exp 


data Vals a = Esp VarDisc      |
              Vari VarDisc     |
              Desv Expr        |
              P VarDisc Expr   |
              Ps Expr Expr Expr

data Expr = VarDisc | VarCont | Vals


data Value = NumInt Int       |
             NumFloat Float   
            -- LInt  Seq[NumInt]    |
            -- LFloat Seq[NumFloat] 
             



data Decl a = Decl {
    name :: Name,
    expr :: a
}

type Ngh = (Name, Float)
type Node = (Name, [Ngh])
type MkL  = [Node]

data Op = Lt | Gt | Lte | Gte | Eq | NEq

data Exp a where
  
  -- Expresiones enteras
  Const  :: Num -> Exp Num
  Var    :: Variable -> Exp Num
  VarA   :: Variable -> Exp VarAle
  UMinus :: Exp Num -> Exp Num
  Plus   :: Exp Num -> Exp Num -> Exp Num
  Minus  :: Exp Num -> Exp Num -> Exp Num
  Div    :: Exp Num -> Exp Num -> Exp Num
  Acces  :: Exp LNum -> Exp Num -> Exp Num

  Bin    :: Exp Int -> Exp Float -> Exp VarDisc
  Poiss  :: Exp Int -> Exp VarDisc  
  Geo    :: Exp Float -> Exp VarDisc        
  Pasc   :: Exp Int -> Exp Float -> Exp VarDisc
  Hiper  :: Exp Int -> Exp Int -> Exp Int ->  Exp VarDisc
  T      :: Exp LInt -> Exp LFloat -> Exp VarDisc

  Norm   :: Exp Float -> Exp Float -> Exp VarCont
  Unif   :: Exp Float -> Exp Float -> Exp VarCont
  Expo   :: Exp Float -> Exp VarCont

  Esp    :: Exp VarAle -> Exp Num
  Vari   :: Exp VarAle -> Exp Num
  Desv   :: Exp VarAle -> Exp Num

  POp    :: Exp VarAle -> Exp Op -> Exp Num -> Exp Num
  POpBt  :: Exp VarAle -> Exp Op -> Exp Num -> Exp Op -> Exp Num -> Exp Num

  MaxP   :: Exp VarDisc -> Exp LInt
  MinP   :: Exp VarDisc -> Exp LInt
  

  Node   :: Name -> Exp [Ngh] ->    
  MkL    :: Exp [Node] -> Exp Mk

  -- Expresiones booleana

deriving instance Show (Exp a)
deriving instance Eq (Exp a)


data Comm = Let Variable (Exp Num)     |
            Foll Variable (Exp VarAle) |
            Plot (Exp VarAle)
