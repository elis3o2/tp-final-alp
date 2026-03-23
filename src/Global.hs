module Global where
import AST
import qualified Data.Map as M


type EnvAle = M.Map Name VarAle
type EnvVec = M.Map Name (Vec NumC)
type EnvNum = M.Map Name NumC

data Env = Env {ale :: EnvAle, vec :: EnvVec, num :: EnvNum}

-- | Valor del estado inicial
initialEnv :: Env
initialEnv = Env M.empty M.empty M.empty


data Conf = Conf {
    verbose :: Bool   -- si se muestra información extra
}

defaultConf :: Conf 
defaultConf = Conf {verbose=True}