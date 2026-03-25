{-|
Module      : Errors
Description : Definición del tipo de los errores
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Error where

import Text.Parsec.Error ( ParseError )


data Error
  = ParseErr ParseError
  | ExecErr EError
  deriving (Show)

data EError
  = DivByZero
  | NotDefined
  | IntValueExpected
  | ContVarExpected
  | DiscVarExpected
  | AleInvalidForm
  | ProbInvalidForm
  | InvalidProb
  | InvalidIndex
  | InvalidVarType
  | VarNotScoope

  deriving (Eq)

instance Show EError where
  show DivByZero        = "Division by zero"
  show NotDefined       = "Function not defined"
  show IntValueExpected = "Int value was expected"
  show ContVarExpected  = "Continuous variable was expected"
  show DiscVarExpected  = "Discrete variable was expected"
  show ProbInvalidForm  = "Ranges at probability function not valid"
  show AleInvalidForm   = "Random Variable form not valid"
  show InvalidProb      = "Probability not valid"
  show InvalidIndex     = "Index not valid"
  show InvalidVarType   = "Variable type not adimited"
  show VarNotScoope     = "Variable not in scoope"




