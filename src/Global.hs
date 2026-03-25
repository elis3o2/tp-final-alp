module Global where
import AST
import qualified Data.Map as M



type Env = M.Map Name Value

-- | Valor del estado inicial
initialEnv :: Env
initialEnv = M.empty


data Conf = Conf {
    verbose :: Bool   -- si se muestra información extra
}

defaultConf :: Conf 
defaultConf = Conf {verbose=True}