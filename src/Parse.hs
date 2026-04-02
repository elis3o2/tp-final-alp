{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse where

import AST
import Common

import Prelude hiding ( const )
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import Data.Char (isUpper, isLower)
import qualified Data.Vector as V
import Eval (evalPathExp)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Tokens Analizer
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef
  { commentStart   = "/*"
  , commentEnd     = "*/"
  , commentLine    = "//"
  , reservedNames  =
      [ "Bin", "Binomial"
      , "Poi", "Poisson"
      , "Geo", "Geometrica"
      , "BN", "Pascal"
      , "HG", "Hipergeometrica"
      , "N", "Normal"
      , "Exp", "Exponencial"
      , "Unif", "Uniforme"
      , "P", "E", "SD", "V", "fdp", "F"
      , "moda", "antimoda", "maxP", "minP", "maxFDP"
      , "mk" ,"print", "table", "plot",
       "ex", "stationary", "simulate", "start", "prob"
      ]
  , reservedOpNames =
      [ "=", ",", "->", "<-", "~"
      , "<=", "<", ">=", ">", ":="
      , "+", "-", "*", "/"
      ]
  }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

double :: P Double
double = Tok.float lexer

parens :: P a -> P a
parens = Tok.parens lexer

brackets :: P a -> P a
brackets = Tok.brackets lexer

braces :: P a -> P a
braces = Tok.braces lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

upperString :: P String
upperString = Tok.lexeme lexer $ try $ do
  s@(c:_) <- identifier
  if isUpper c
    then return s
    else unexpected "uppercase identifier was expected"

lowerString :: P String
lowerString = Tok.lexeme lexer $ try $ do
  s@(c:_) <- identifier
  if isLower c
    then return s
    else unexpected "lowercase identifier was expected"


parseNameL :: P Name
parseNameL = lowerString

parseNameU :: P Name
parseNameU = upperString

parseVarL :: P Exp
parseVarL = VarRef <$> parseNameL

parseVarU :: P Exp
parseVarU = VarRef <$> parseNameU


-- *********************************************************** +
-- Numeric                                                     |
-- *********************************************************** +
parseConstNum :: P Exp                                      -- |
parseConstNum = ConstN <$> double                           -- |
                                                            -- |
parseUMinus :: P Exp                                        -- |
parseUMinus = do reservedOp "-"                             -- |
                 UMinus <$> parseNumBaseAtom                -- |
                                                            -- |
parseAddOp :: P (Exp -> Exp -> Exp)                         -- |
parseAddOp = (reservedOp "+"  >> return (OpNum Plus))       -- |
         <|> (reservedOp "-" >> return (OpNum Minus))       -- |
                                                            -- |
parseMulOp :: P (Exp -> Exp -> Exp)                         -- |
parseMulOp = (reservedOp "*" >> return (OpNum Times))       -- |
         <|> (reservedOp "/" >> return (OpNum Div))         -- |
-------------------------------------------------------------- +



-- *********************************************************** +
-- Vectors                                                     |
-- *********************************************************** +
parseVector :: P (Vec Exp)                                  -- |
parseVector = V.fromList                                    -- |
          <$> parens (parseNumExp `sepBy1` reservedOp ",")  -- |
                                                            -- |
parseConstVec :: P Exp                                      -- |
parseConstVec = ConstV <$> parseVector                      -- |
                                                            -- |
parseAccess :: P Exp                                        -- |
parseAccess = do v <- parseVecExp                           -- |
                 n <- brackets parseNumExp                  -- |
                 return (Access v n)                        -- |
-------------------------------------------------------------- +


-- ***************************************************
-- Random variables                                  |
-- ***************************************************
-- Discrete                                          |
---------------------------------------------------- |
                                                  -- |
parseRandDisc :: P RandExp                        -- |
parseRandDisc = try parseBin                      -- |
            <|> try parsePoiss                    -- |
            <|> try parseGeo                      -- |
            <|> try parsePasc                     -- |
            <|> try parseHiper                    -- |
            <|> parseCustom                       -- |
                                                  -- |
parseBin :: P RandExp                             -- |
parseBin = do try (reserved "Bin")                -- |
                <|> reserved "Binomial"           -- |
              parens $ do                         -- |
                n <- parseNumExp                  -- |
                reservedOp ","                    -- |
                p <- parseNumExp                  -- |
                return (DiscE (BinE n p))         -- |
                                                  -- |
parsePoiss :: P RandExp                           -- |
parsePoiss = do try (reserved "Poi")              -- |
                  <|> reserved "Poisson"          -- |
                parens $ do                       -- |
                    n <- parseNumExp              -- |
                    return (DiscE (PoissE n))     -- |
                                                  -- |
parseGeo :: P RandExp                             -- |
parseGeo = do try (reserved "Geo")                -- |
                <|> reserved "Geometrica"         -- |
              parens $ do                         -- |
                  p <- parseNumExp                -- |
                  return (DiscE (GeoE p))         -- |
                                                  -- |
parsePasc :: P RandExp                            -- |
parsePasc = do try (reserved "BN")                -- |
                <|> reserved "Pascal"             -- |
               parens $ do                        -- |
                  n <- parseNumExp                -- |
                  reservedOp ","                  -- |
                  p <- parseNumExp                -- |
                  return (DiscE (PascE n p))      -- |
                                                  -- |
parseHiper :: P RandExp                           -- |
parseHiper = do try (reserved "HG")               -- |
                  <|> reserved "Hipergeometrica"  -- |
                parens $ do                       -- |
                    n <- parseNumExp              -- |
                    reservedOp ","                -- |
                    m <- parseNumExp              -- |
                    reservedOp ","                -- |
                    r <- parseNumExp              -- |
                    return (DiscE (HiperE n m r)) -- |
                                                  -- |
parseCustom :: P RandExp                          -- |
parseCustom = brackets $ do                       -- |
                  v1 <- parseVecExp               -- |
                  reservedOp ","                  -- |
                  v2 <- parseVecExp               -- |
                  return (DiscE (CustomE v1 v2))  -- |
                                                  -- |
---------------------------------------------------- +
-- Continuous                                        |
---------------------------------------------------- +
                                                  -- |
parseRandCont :: P RandExp                        -- |
parseRandCont = try parseNorm                     -- |
            <|> try parseUnif                     -- |
            <|> parseExpo                         -- |
                                                  -- |
parseNorm :: P RandExp                            -- |
parseNorm = do try (reserved "N")                 -- |
                <|> reserved "Normal"             -- |
               parens $ do                        -- |
                 m <- parseNumExp                 -- |
                 reservedOp ","                   -- |
                 u <- parseNumExp                 -- |
                 return (ContE (NormE m u))       -- |
                                                  -- |
parseExpo :: P RandExp                            -- |
parseExpo = do try (reserved "Exp")               -- |
                <|> reserved "Exponencial"        -- |
               parens $ do                        -- |
                  a <- parseNumExp                -- |
                  return (ContE (ExpoE a))        -- |
                                                  -- |
parseUnif :: P RandExp                            -- |
parseUnif = do try (reserved "Unif")              -- |
                <|> reserved "Uniforme"           -- |
               parens $ do                        -- |
                  a <- parseNumExp                -- |
                  reservedOp ","                  -- |
                  b <- parseNumExp                -- |
                  return (ContE (UnifE a b))      -- |
---------------------------------------------------- +



-- ****************************************************** +
-- Markov Chains                                          |
-- ****************************************************** +
parseMarkov :: P MarkovExp                             -- |
parseMarkov = do reserved "mk"                         -- |
                 l <- parseNames                       -- |
                 return (MarkovE l)                    -- |
                                                       -- |
                                                       -- |
parseNextDist :: P Exp                                 -- |
parseNextDist = do reserved "ex"                       -- |
                   parens $ do                         -- |
                    x <- parseMarkovExp                -- |
                    reservedOp ","                     -- |
                    n <- parseNumExp                   -- |
                    return (NextDist x n)              -- |
                                                       -- |
parseStationary :: P Exp                               -- |
parseStationary = do reserved "stationary"             -- |
                     x <- parens parseMarkovExp        -- |
                     return (Stationary x)             -- |
--------------------------------------------------------- +
-- Nodes                                                  |
--------------------------------------------------------- +
parseNeight :: P (Name, Exp)                           -- |
parseNeight = do parens $ do                           -- |
                  n <- identifier                      -- |
                  reservedOp ","                       -- |
                  p <- parseNumExp                     -- |
                  return (n, p)                        -- |
                                                       -- |
parseNode :: P NodeExp                                 -- |
parseNode = do l <- brackets                           -- |
                (parseNeight `sepBy1` reservedOp ",")  -- |
               return (NE l)                           -- |
--------------------------------------------------------- +





-- *********************************************** +
-- Stats                                           |
-- *********************************************** +
                                                -- |
-- Numeric Stats                                -- |
parseNumStat :: P Exp                           -- |
parseNumStat = try parseMean                    -- |
           <|> try parseVariance                -- |
           <|> try parseStdDev                  -- |
           <|> try parseFdp                     -- |
           <|> try parseMaxP                    -- |
           <|> parseMaxFDP                      -- |
                                                -- |
-------------------------------------------------- |
-- Vector Stats                                 -- |
parseVecStat :: P Exp                           -- |
parseVecStat = try parseSMode                   -- |
-------------------------------------------------- |
                                                -- |
parseMean :: P Exp                              -- |
parseMean = do reserved "E"                     -- |
               v <- parens parseRandExp         -- |
               return (Mean v)                  -- |
                                                -- |
parseVariance :: P Exp                          -- |
parseVariance = do reserved "V"                 -- |
                   v <- parens parseRandExp     -- |
                   return (Variance v)          -- |
                                                -- |
parseStdDev :: P Exp                            -- |
parseStdDev = do reserved "SD"                  -- |
                 v <- parens parseRandExp       -- |
                 return (StdDev v)              -- |
                                                -- |
parseFdp :: P Exp                               -- |
parseFdp = do reserved "fdp"                    -- |
              parens $ do                       -- |
                v <- parseRandExp               -- |
                reservedOp ","                  -- |
                n <- parseNumExp                -- |
                return (FDP v n)                -- |
                                                -- |
parseMaxP :: P Exp                              -- |
parseMaxP = do reserved "maxP"                  -- |
               v <- parens parseRandExp         -- |
               return (MaxP v)                  -- |
                                                -- |
parseMaxFDP :: P Exp                            -- |
parseMaxFDP = do reserved "maxFDP"              -- |
                 v <- parens parseRandExp       -- |
                 return (MaxFDP v)              -- |
                                                -- |
parseSMode :: P Exp                             -- |
parseSMode = do reserved "moda"                 -- |
                v <- parens parseRandExp        -- |
                return (Mode v)                 -- |
-------------------------------------------------- +


-- *********************************************************** +
-- Path                                                        |
-- *********************************************************** +
parsePath :: P Path                                         -- |
parsePath = V.fromList                                      -- |
          <$> brackets (identifier `sepBy1` reservedOp ",") -- |
                                                            -- |
parseNames :: P Path                                        -- |
parseNames = V.fromList                                     -- |
          <$> parens (identifier `sepBy1` reservedOp ",")   -- |
-------------------------------------------------------------- +



-- *****************************************************
-- Probabilitys                                        |
-- *****************************************************
prob :: P Exp                                       -- |
prob = try probRand <|> probMk                      -- |
------------------------------------------------------ +
--  Random Variables Probabilitys                      |
------------------------------------------------------ +
probRand :: P Exp                                   -- |
probRand = do reserved "P"                          -- |
              parens parseProbRand                  -- |
                                                    -- |
parseProbRand :: P Exp                              -- |
parseProbRand = try parsePOpBt <|> parsePOp         -- |
                                                    -- |
parsePOp :: P Exp                                   -- |
parsePOp = try (do n <- parseNumExp                 -- |
                   op <- parseOpositte              -- |
                   v <- parseRandExp                -- |
                   return (Prob v op n))            -- |
      <|> do v <- parseRandExp                      -- |
             op <- parseOp                          -- |
             n <- parseNumExp                       -- |
             return (Prob v op n)                   -- |
                                                    -- |
parsePOpBt :: P Exp                                 -- |
parsePOpBt = do n <- parseNumExp                    -- |
                op1 <- parseOpositte                -- |
                v <- parseRandExp                   -- |
                op2 <- parseOp                      -- |
                m <- parseNumExp                    -- |
                return (ProbBetween v op1 n op2 m)  -- |
                                                    -- |
------------------------------------------------------ +
-- Operations Symbols                                  |
------------------------------------------------------ |
parseOp :: P OpComp                                 -- |
parseOp = try (reservedOp "<=" >> return Lte)       -- |
      <|> try (reservedOp "<"  >> return Lt)        -- |
      <|> try (reservedOp ">=" >> return Gte)       -- |
      <|> try (reservedOp ">"  >> return Gt)        -- |
      <|> try (reservedOp "/=" >> return NEq)       -- |
      <|> (reservedOp "=" >> return Eq)             -- |
                                                    -- |
parseOpositte :: P OpComp                           -- |
parseOpositte = try (reservedOp "<=" >> return Gte) -- |
      <|> try (reservedOp "<"  >> return Gt)        -- |
      <|> try (reservedOp ">=" >> return Lte)       -- |
      <|> try (reservedOp ">"  >> return Lt)        -- |
      <|> try (reservedOp "/=" >> return NEq)       -- |
      <|> (reservedOp "=" >> return Eq)             -- |
                                                    -- |
------------------------------------------------------ +
-- Markov Probabilitys                                 |
------------------------------------------------------ +
probMk :: P Exp                                     -- |
probMk = do reserved "F"                            -- |
            parseProbMk                             -- |
                                                    -- |
parseProbMk :: P Exp                                -- |
parseProbMk = try parseProbStep <|>                 -- |
              try parseProbPath <|>                 -- |
              parseProbHit                          -- |
                                                    -- |
parseProbStep :: P Exp                              -- |
parseProbStep = do n <- parseNumExp                 -- |
                   parens $ do                      -- |
                    x <- parseMarkovExp             -- |
                    reservedOp ","                  -- |
                    i <- identifier                 -- |
                    reservedOp ","                  -- |
                    j <-identifier                  -- |
                    reservedOp ","                  -- |
                    return (ProbStep x i j n)       -- |
                                                    -- |
parseProbPath :: P Exp                              -- |
parseProbPath = do parens $ do                      -- |
                    x <- parseMarkovExp             -- |
                    reservedOp ","                  -- |
                    c <- parsePathExp               -- |
                    return (ProbPath x c)           -- |
                                                    -- |
parseProbHit :: P Exp                               -- |
parseProbHit = do parens $ do                       -- |
                    x <- parseMarkovExp             -- |
                    i <- identifier                 -- |
                    reservedOp ","                  -- |
                    j <- identifier                 -- |
                    return (ProbHit x i j)          -- |
------------------------------------------------------ +


-- ***************************************************** +
-- Simulations                                           |
-- ***************************************************** +
parseSimulateName :: P Exp                            -- |
parseSimulateName = do reserved "simulate"            -- |
                       parens $ do                    -- |
                        x <- parseMarkovExp           -- |
                        reservedOp ","                -- |
                        n <- parseNumExp              -- |
                        reservedOp ","                -- |
                        reserved "start"              -- |
                        reservedOp "="                -- |
                        m <- identifier               -- |
                        return (SimulFromName x m n)  -- |
                                                      -- |
parseSimulateVec :: P Exp                             -- |
parseSimulateVec = do reserved "simulate"             -- |
                      parens $ do                     -- |
                        x <- parseMarkovExp           -- |
                        reservedOp ","                -- |
                        n <- parseNumExp              -- |
                        reservedOp ","                -- |
                        reserved "prob"               -- |
                        reservedOp "="                -- |
                        v <- parseVecExp              -- |
                        return (SimulFromVec x v n)   -- |
-------------------------------------------------------- +



-- ****************************************************** +
-- Expressions                                            |
-- ****************************************************** +
parseExp :: P Exp                                      -- |
parseExp =                                             -- |
      try parseNumExp                                  -- |
  <|> try parseVecExp                                  -- |
  <|> try parseRandExp                                 -- |
  <|> try parseMarkovExp                               -- |
  <|> parsePathExp                                     -- |
                                                       -- |
--------------------------------------------------------- +
-- Numeric                                                |
--------------------------------------------------------- +
parseNumBaseAtom :: P Exp                              -- |
parseNumBaseAtom =                                     -- |
      try prob                                         -- |
  <|> try parseAccess                                  -- |
  <|> try parseNumStat                                 -- |
  <|> try parseConstNum                                -- |
  <|> try parseVarL                                    -- |
  <|> parens parseNumExp                               -- |
                                                       -- |
parseNumAtom :: P Exp                                  -- |
parseNumAtom =                                         -- |
      try parseUMinus                                  -- |
  <|> parseNumBaseAtom                                 -- |
                                                       -- |
parseNumTerm :: P Exp                                  -- |
parseNumTerm = chainl1 parseNumAtom parseMulOp         -- |
                                                       -- |
parseNumExp :: P Exp                                   -- |
parseNumExp = chainl1 parseNumTerm parseAddOp          -- |
                                                       -- |
--------------------------------------------------------- +
-- Vectors                                                |
--------------------------------------------------------- +
parseVecExp :: P Exp                                   -- |
parseVecExp = try parseConstVec                        -- |
          <|> try parseVarL                            -- |
          <|> parseVecStat                             -- |
                                                       -- |
--------------------------------------------------------- +
-- Random                                                 |
--------------------------------------------------------- +
parseRandExp :: P Exp                                  -- |
parseRandExp = try                                     -- |
     parseVarU                                         -- |
 <|> try (Rand <$> parseRandDisc)                      -- |
 <|> (Rand <$> parseRandCont)                          -- |
                                                       -- |
--------------------------------------------------------- +
-- Path                                                   |
--------------------------------------------------------- +
parsePathExp :: P Exp                                  -- |
parsePathExp = try (ConstCh <$> parsePath)             -- |
            <|> try parseVarL                          -- |
            <|> try parseSimulateName                  -- |
            <|> parseSimulateVec                       -- |
                                                       -- |
--------------------------------------------------------- +
-- Markov                                                 |
--------------------------------------------------------- +
parseMarkovExp :: P Exp                                -- |
parseMarkovExp = try (Markov <$> parseMarkov)          -- |
              <|> try parseVarU                        -- |
              <|> try parseNextDist                    -- |
              <|> parseStationary                      -- |
--------------------------------------------------------- +


-- ****************************************************** +
-- Commands                                               |
-- ****************************************************** +
parseComm :: P Comm                                    -- |
parseComm =  parseDec                                  -- |
          <|> try parsePrintNum                        -- |
          <|> try parseTable                           -- |
          <|> parsePlot                                -- |
                                                       -- |
--------------------------------------------------------- +
-- Declarations                                           |
--------------------------------------------------------- +
parseDec :: P Comm                                     -- |
parseDec = try parseDecNode                            -- |
       <|> try parseDecNum                             -- |
       <|> try parseDecAle                             -- |
       <|> try parseDecVec                             -- |
       <|> try parseDecPath                            -- |
       <|> parseDecMk                                  -- |
                                                       -- |
parseDecNum :: P Comm                                  -- |
parseDecNum = do x <- lowerString                      -- |
                 reservedOp "="                        -- |
                 e <- parseNumExp                      -- |
                 return (Let x e)                      -- |
                                                       -- |
parseDecAle :: P Comm                                  -- |
parseDecAle = do x <- upperString                      -- |
                 reservedOp "~" <|> reservedOp ":="    -- |
                 e <- parseRandExp                     -- |
                 return (Let x e)                      -- |
                                                       -- |
parseDecVec :: P Comm                                  -- |
parseDecVec = do x <- lowerString                      -- |
                 reservedOp "="                        -- |
                 e <- parseVecExp                      -- |
                 return (Let x e)                      -- |
                                                       -- |
parseDecMk :: P Comm                                   -- |
parseDecMk = do x <- upperString                       -- |
                reservedOp "="                         -- |
                e <- parseMarkovExp                    -- |
                return (Let x e)                       -- |
                                                       -- |
parseDecPath :: P Comm                                 -- |
parseDecPath = do x <- lowerString                     -- |
                  reservedOp "="                       -- |
                  e <- parsePathExp                    -- |
                  return (Let x e)                     -- |
                                                       -- |
parseDecNode :: P Comm                                 -- |
parseDecNode = do x <- identifier                      -- |
                  reservedOp "->"                      -- |
                  e <- parseNode                       -- |
                  return (LetN x e)                    -- |
                                                       -- |
--------------------------------------------------------- +
-- Print                                                  |
--------------------------------------------------------- +
parsePrintNum :: P Comm                                -- |
parsePrintNum = do reserved "print"                    -- |
                   e <- parens parseExp                -- |
                   return(Print e)                     -- |
                                                       -- |
--------------------------------------------------------- +
-- Table                                                  |
--------------------------------------------------------- +
parseTable :: P Comm                                   -- |
parseTable = do reserved "table"                       -- |
                parens $ do                            -- |
                  x <- parseRandExp                    -- |
                  (do reservedOp ","                   -- |
                      n <- parseNumExp                 -- |
                      reservedOp ","                   -- |
                      m <- parseNumExp                 -- |
                      return (TableR x n m))           -- |
                   <|> return (Table x)                -- |
                                                       -- |
--------------------------------------------------------- +
-- Plot                                                -- |
--------------------------------------------------------- +
parsePlot :: P Comm                                    -- |
parsePlot = do reserved "plot"                         -- |
               x <- parens parseRandExp                -- |
               return (Plot x)                         -- |
--------------------------------------------------------- +

        

-- | Parser de programas (listas de declaraciones) 
program :: P [Comm]
program =  many parseComm

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s



parseProgram :: P [Comm]
parseProgram = whiteSpace *> many parseComm <* eof