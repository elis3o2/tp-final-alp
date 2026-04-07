{-|
Module      : Global
Description : Global state definition
-}
module Global where
import AST
import qualified Data.Map as M
import Common


data Env = Env {decls::M.Map Name (Value, Type), nodes::M.Map Name NodeVal} deriving (Show)

-- | Initial state
initialEnv :: Env
initialEnv = Env M.empty M.empty


data Conf = Conf {
    verbose  :: Bool,
    decimals :: Int
} deriving (Show)

-- | Default Configuration
defaultConf :: Conf 
defaultConf = Conf {verbose=True, decimals=6}