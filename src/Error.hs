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
  | TypeErr TError 
  deriving (Show)

data EError
  = DivByZero
  | NotDefined
  | IntValueExpected
  | RandInvalidForm
  | ProbInvalidForm
  | InvalidProb
  | InvalidIndex
  | TypeCheckError
  | InvalidRanges
  | InvalidSteps
  | InvalidName
  | InvalidVector

  deriving (Eq)


data TError =
    ContExpExpected
  | DiscExpExpected
  | VecExpExpected
  | RandExpExpected
  | NumExpExpected
  | PathExpExpected
  | VarNotScoope
  | InvalidVarType
  | EmptyPath
  | MkExpExpected


instance Show EError where
  show DivByZero        = "Division by zero"
  show NotDefined       = "Function not defined"
  show IntValueExpected = "Int value was expected"
  show ProbInvalidForm  = "Ranges at probability function not valid"
  show RandInvalidForm  = "Random Variable form not valid"
  show InvalidProb      = "Probability not valid"
  show InvalidRanges    = "Probability functions not in valid range"
  show InvalidIndex     = "Index not valid"
  show TypeCheckError   = "Something went wrong with the TypeChecker"
  show InvalidSteps     = "Number of steps is not valid"
  show InvalidName      = "Name is not part of the Markov Chain"  
  show InvalidVector    = "Vector provided is not valid"
instance Show TError where
  show InvalidVarType   = "Variable type not adimited"
  show VarNotScoope     = "Variable not in scoope"
  show ContExpExpected  = "Continuous expression was expected"
  show DiscExpExpected  = "Discrete expression was expected"
  show NumExpExpected   = "Numeric expression was expected"
  show VecExpExpected   = "Vector expression was expected"
  show RandExpExpected  = "Random expression was expected"
  show PathExpExpected = "Path expression was expected"
  show EmptyPath       = "Paths can´t be empyt"
  show MkExpExpected    = "Markov Expression was expected"