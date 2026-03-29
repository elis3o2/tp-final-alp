module Parse where

import AST
import Common

import Prelude hiding ( const )
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)
import Data.Char (isUpper, isLower)
import qualified Data.Vector as V

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
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
      , "P", "E", "SD", "V", "fdp"
      , "moda", "antimoda", "maxP", "minP", "maxFDP"
      , "mk" ,"print", "table", "plot"
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
    else unexpected "se esperaba identificador con mayúscula"

lowerString :: P String
lowerString = Tok.lexeme lexer $ try $ do
  s@(c:_) <- identifier
  if isLower c
    then return s
    else unexpected "se esperaba identificador con minúscula"

-----------------------
-- Números
-----------------------

parseNumV :: P NumC
parseNumV = try (D <$> double) <|> (I . fromInteger <$> natural)

parseConstNum :: P Exp
parseConstNum = ConstN <$> parseNumV

parseNameL :: P Name
parseNameL = lowerString

parseNameU :: P Name
parseNameU = upperString

parseVarL :: P Exp
parseVarL = VarRef <$> parseNameL

parseVarU :: P Exp
parseVarU = VarRef <$> parseNameU


parseUMinus :: P Exp
parseUMinus = do reservedOp "-"
                 UMinus <$> parseNumBaseAtom

parseAddOp :: P (Exp -> Exp -> Exp)
parseAddOp = (reservedOp "+"  >> return (OpNum Plus))
         <|> (reservedOp "-" >> return (OpNum Minus))

parseMulOp :: P (Exp -> Exp -> Exp)
parseMulOp = (reservedOp "*" >> return (OpNum Times))
         <|> (reservedOp "/" >> return (OpNum Div))

-----------------------
-- Expresiones numéricas
-----------------------

parseNumBaseAtom :: P Exp
parseNumBaseAtom =
      try prob
  <|> try parseAccess
  <|> try parseNumFunc
  <|> try parseConstNum
  <|> try parseVarL
  <|> parens parseNumExp

parseNumAtom :: P Exp
parseNumAtom =
      try parseUMinus
  <|> parseNumBaseAtom

parseNumTerm :: P Exp
parseNumTerm = chainl1 parseNumAtom parseMulOp

parseNumExp :: P Exp
parseNumExp = chainl1 parseNumTerm parseAddOp

---------------------------
-- Probabilidades
---------------------------

prob :: P Exp
prob = do reserved "P"
          parens parseProb

parseProb :: P Exp
parseProb = try parsePOpBt <|> parsePOp

parsePOp :: P Exp
parsePOp = try (do n <- parseNumExp
                   op <- parseOp
                   v <- parseAleExp
                   return (Prob v op n))
      <|> do v <- parseAleExp
             op <- parseOp
             n <- parseNumExp
             return (Prob v op n)

parsePOpBt :: P Exp
parsePOpBt = do n <- parseNumExp
                op1 <- parseOp
                v <- parseAleExp
                op2 <- parseOp
                m <- parseNumExp
                return (ProbBetween v op1 n op2 m)

parseOp :: P OpComp
parseOp = try (reservedOp "<=" >> return Lte)
      <|> try (reservedOp "<"  >> return Lt)
      <|> try (reservedOp ">=" >> return Gte)
      <|> try (reservedOp ">"  >> return Gt)
      <|> try (reservedOp "/=" >> return NEq)
      <|> (reservedOp "=" >> return Eq)

-----------------------
-- Vectores
-----------------------

parseVector :: P (Vec Exp)
parseVector = V.fromList <$> parens (parseNumExp `sepBy1` reservedOp ",")

parseConstVec :: P Exp
parseConstVec = ConstV <$> parseVector


parseVecFunc :: P Exp
parseVecFunc = try parseModa

parseVecAtom :: P Exp
parseVecAtom = try parseConstVec <|> try parseVarL <|> parseVecFunc

parseVecExp :: P Exp
parseVecExp = parseVecAtom

parseAccess :: P Exp
parseAccess = do
  v <- parseVecExp
  n <- brackets parseNumExp
  return (Access v n)

----------------------
-- Variables aleatorias
----------------------

parseAleExp :: P Exp
parseAleExp = try parseVarU <|>
 try (Rand <$> parseVarDisc) <|> (Rand <$> parseVarCont)



----------------------
-- Discretas
----------------------

parseVarDisc :: P RandExp
parseVarDisc = try parseBin
           <|> try parsePoiss
           <|> try parseGeo
           <|> try parsePasc
           <|> try parseHiper
           <|> parseCustom

parseBin :: P RandExp
parseBin = do
  try (reserved "Bin") <|> reserved "Binomial"
  parens $ do
    n <- parseNumExp
    reservedOp ","
    p <- parseNumExp
    return (DiscE (BinE n p))

parsePoiss :: P RandExp
parsePoiss = do try (reserved "Poi") <|> reserved "Poisson"
                parens $ do
                    n <- parseNumExp
                    return (DiscE (PoissE n))

parseGeo :: P RandExp
parseGeo = do try (reserved "Geo") <|> reserved "Geometrica"
              parens $ do
                  p <- parseNumExp
                  return (DiscE (GeoE p))

parsePasc :: P RandExp
parsePasc = do try (reserved "BN") <|> reserved "Pascal"
               parens $ do
                  n <- parseNumExp
                  reservedOp ","
                  p <- parseNumExp
                  return (DiscE (PascE n p))

parseHiper :: P RandExp
parseHiper = do try (reserved "HG") <|> reserved "Hipergeometrica"
                parens $ do
                    n <- parseNumExp
                    reservedOp ","
                    m <- parseNumExp
                    reservedOp ","
                    r <- parseNumExp
                    return (DiscE (HiperE n m r))

parseCustom :: P RandExp
parseCustom = brackets $ do
  v1 <- parseVecExp
  reservedOp ","
  v2 <- parseVecExp
  return (DiscE (CustomE v1 v2))

----------------------
-- Continuas
----------------------

parseVarCont :: P RandExp
parseVarCont = try parseNorm <|> try parseUnif <|> parseExpo

parseNorm :: P RandExp
parseNorm = do try (reserved "N") <|> reserved "Normal"
               parens $ do
                 m <- parseNumExp
                 reservedOp ","
                 u <- parseNumExp
                 return (ContE (NormE m u))
parseExpo :: P RandExp
parseExpo = do try (reserved "Exp") <|> reserved "Exponencial"
               parens $ do
                  a <- parseNumExp
                  return (ContE (ExpoE a))

parseUnif :: P RandExp
parseUnif = do try (reserved "Unif") <|> reserved "Uniforme"
               parens $ do
                  a <- parseNumExp
                  reservedOp ","
                  b <- parseNumExp
                  return (ContE (UnifE a b))


----------------------
-- Funciones numéricas sobre aleatorias
----------------------

parseNumFunc :: P Exp
parseNumFunc = try parseEsp
           <|> try parseVari
           <|> try parseDesv
           <|> try parseFdp
           <|> try parseMaxP
           <|> parseMaxFDP

parseEsp :: P Exp
parseEsp = do reserved "E"
              v <- parens parseAleExp
              return (Mean v)

parseVari :: P Exp
parseVari = do reserved "V"
               v <- parens parseAleExp
               return (Variance v)

parseDesv :: P Exp
parseDesv = do reserved "SD"
               v <- parens parseAleExp
               return (StdDev v)

parseFdp :: P Exp
parseFdp = do reserved "fdp"
              parens $ do
                v <- parseAleExp
                reservedOp ","
                n <- parseNumExp
                return (FDP v n)

parseMaxP :: P Exp
parseMaxP = do reserved "maxP"
               v <- parens parseAleExp
               return (MaxP v)


parseMaxFDP :: P Exp
parseMaxFDP = do reserved "maxFDP"
                 v <- parens parseAleExp
                 return (MaxFDP v)

----------------------
-- Funciones vectoriales
----------------------

parseModa :: P Exp
parseModa = do reserved "moda"
               v <- parens parseAleExp
               return (Mode v)





parseExp :: P Exp
parseExp = try parseNumExp <|> try parseVecExp <|> parseAleExp


----------------------
-- Comandos
----------------------

parseDecNum :: P Comm
parseDecNum = do x <- lowerString
                 reservedOp "="
                 e <- parseNumExp 
                 return (Let x e)

parseDecAle :: P Comm
parseDecAle = do x <- upperString
                 reservedOp "~" <|> reservedOp ":="
                 e <- parseAleExp
                 return (Let x e)

parseDecVec :: P Comm
parseDecVec = do x <- lowerString
                 reservedOp "="
                 e <- parseVecExp
                 return (Let x e)

parseDecMk :: P Comm
parseDecMk = do x <- upperString
                reservedOp "="
                e <- parseMk
                return (Let x e)


parsePrintNum :: P Comm 
parsePrintNum = do reserved "print"
                   e <- parens parseExp
                   return(Print e)

parseTable :: P Comm 
parseTable = do reserved "table"
                parens $ do
                  x <- parseAleExp
                  (do reservedOp ","
                      n <- parseNumExp
                      reservedOp ","
                      m <- parseNumExp
                      return (TableR x n m))
                   <|> return (Table x) 


parsePlot :: P Comm
parsePlot = do reserved "plot"
               x <- parens parseAleExp
               return (Plot x) 


parseNeight :: P (Name, Exp)
parseNeight = do parens $ do
                  n <- identifier
                  reservedOp ","
                  p <- parseNumExp
                  return (n,p) 

parseNode :: P Comm 
parseNode = do n <- identifier
               reservedOp "->"
               brackets $ do
                l <- (parseNeight `sepBy1` reservedOp ",")
                return (Let n (Node l))

parseMk :: P Exp  
parseMk = do reserved "mk"
             l <- parens (identifier `sepBy1` reservedOp ",")
             return (MkE l)

parseComm :: P Comm
parseComm =   try parseDecNum <|> try parseDecAle <|> try parseDecVec
          <|> try parsePrintNum <|> try parseTable <|> try parsePlot 
          <|> try parseNode <|> parseDecMk


-- | Parser de programas (listas de declaraciones) 
program :: P [Comm]
program =  many parseComm

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s



parseProgram :: P [Comm]
parseProgram = whiteSpace *> many parseComm <* eof