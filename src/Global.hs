module Global where
import AST
import qualified Data.Map as M
import Common


data Env = Env {decls::M.Map Name (Value, Type), ndes::M.Map Name NodeVal}

-- | Valor del estado inicial
initialEnv :: Env
initialEnv = Env M.empty M.empty


data Conf = Conf {
    verbose :: Bool   -- si se muestra información extra
}

defaultConf :: Conf 
defaultConf = Conf {verbose=True}