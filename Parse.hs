module Parse where

import ASL
import Text.Parsec hiding (parse)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Data.Char (isUpper, isLower)
import qualified Data.Vector as V

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef
  { commentStart   = "/*"
  , commentEnd     = "*/"
  , commentLine    = "//"
  , reservedNames  =
      [ "B", "Binomial"
      , "Poi", "Poisson"
      , "Geo", "Geometrica"
      , "BN", "Pascal"
      , "HG", "Hipergeometrica"
      , "N", "Normal"
      , "Exp", "Exponencial"
      , "Unif", "Uniforme"
      , "P", "E", "SD", "V", "fdp"
      , "moda", "antimoda", "maxP", "minP", "maxFDP"
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
double = Tok.double lexer

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

parseNumV :: P NumV
parseNumV = try (D <$> double) <|> (I . fromInteger <$> natural)

parseConstNum :: P ExpNum
parseConstNum = Const <$> parseNumV

parseNameNumV :: P Name
parseNameNumV = lowerString

parseNameVec :: P Name
parseNameVec = lowerString

parseNameAle :: P Name
parseNameAle = upperString

parseNumVar :: P ExpNum
parseNumVar = VNum <$> parseNameNumV

parseUMinus :: P ExpNum
parseUMinus = do reservedOp "-"
                 UMinus <$> parseNumBaseAtom

parseAddOp :: P (ExpNum -> ExpNum -> ExpNum)
parseAddOp = (reservedOp "+"  >> return (OpNum Plus))
         <|> (reservedOp "-" >> return (OpNum Minus))

parseMulOp :: P (ExpNum -> ExpNum -> ExpNum)
parseMulOp = (reservedOp "*" >> return (OpNum Times))
         <|> (reservedOp "/" >> return (OpNum Div))

-----------------------
-- Expresiones numéricas
-----------------------

parseNumBaseAtom :: P ExpNum
parseNumBaseAtom =
      try prob
  <|> try parseAccess
  <|> try parseNumFunc
  <|> try parseConstNum
  <|> try parseNumVar
  <|> parens parseNumExp

parseNumAtom :: P ExpNum
parseNumAtom =
      try parseUMinus
  <|> parseNumBaseAtom

parseNumTerm :: P ExpNum
parseNumTerm = chainl1 parseNumAtom parseMulOp

parseNumExp :: P ExpNum
parseNumExp = chainl1 parseNumTerm parseAddOp

---------------------------
-- Probabilidades
---------------------------

prob :: P ExpNum
prob = do reserved "P"
          parens parseProb

parseProb :: P ExpNum
parseProb = try parsePOpBt <|> parsePOp

parsePOp :: P ExpNum
parsePOp = try (do n <- parseNumExp
                   op <- parseOp
                   v <- parseAleExp
                   return (POp v op n))
      <|> do v <- parseAleExp
             op <- parseOp
             n <- parseNumExp
             return (POp v op n)

parsePOpBt :: P ExpNum
parsePOpBt = do n <- parseNumExp
                op1 <- parseOp
                v <- parseAleExp
                op2 <- parseOp
                m <- parseNumExp
                return (POpBt v op1 n op2 m)

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

parseVector :: P (V.Vector ExpNum)
parseVector = V.fromList <$> parens (parseNumExp `sepBy1` reservedOp ",")

parseConstVec :: P ExpVec
parseConstVec = ConstV <$> parseVector

parseVecName :: P ExpVec
parseVecName = VVec <$> parseNameVec

parseVecFunc :: P ExpVec
parseVecFunc = try parseModa <|> parseAModa

parseVecAtom :: P ExpVec
parseVecAtom = try parseConstVec <|> try parseVecName <|> parseVecFunc

parseVecExp :: P ExpVec
parseVecExp = parseVecAtom

parseAccess :: P ExpNum
parseAccess = do
  v <- parseVecExp
  n <- brackets parseNumExp
  return (Access v n)

----------------------
-- Variables aleatorias
----------------------

parseAleName :: P ExpAle
parseAleName = VAle <$> parseNameAle

parseAleExp :: P ExpAle
parseAleExp = try parseAleName <|> try parseVarDisc <|> parseVarCont

----------------------
-- Discretas
----------------------

parseVarDisc :: P ExpAle
parseVarDisc = try parseBin
           <|> try parsePoiss
           <|> try parseGeo
           <|> try parsePasc
           <|> try parseHiper
           <|> parseCustom

parseBin :: P ExpAle
parseBin = do
  try (reserved "B") <|> reserved "Binomial"
  parens $ do
    n <- parseNumExp
    reservedOp ","
    p <- parseNumExp
    return (Disc (Bin n p))

parsePoiss :: P ExpAle
parsePoiss = do try (reserved "Poi") <|> reserved "Poisson"
                parens $ do
                    n <- parseNumExp
                    return (Disc (Poiss n))

parseGeo :: P ExpAle
parseGeo = do try (reserved "Geo") <|> reserved "Geometrica"
              parens $ do
                  p <- parseNumExp
                  return (Disc (Geo p))

parsePasc :: P ExpAle
parsePasc = do try (reserved "BN") <|> reserved "Pascal"
               parens $ do
                  n <- parseNumExp
                  reservedOp ","
                  p <- parseNumExp
                  return (Disc (Pasc n p))

parseHiper :: P ExpAle
parseHiper = do try (reserved "HG") <|> reserved "Hipergeometrica"
                parens $ do
                    n <- parseNumExp
                    reservedOp ","
                    m <- parseNumExp
                    reservedOp ","
                    r <- parseNumExp
                    return (Disc (Hiper n m r))

parseCustom :: P ExpAle
parseCustom = brackets $ do
  v1 <- parseVecExp
  reservedOp ","
  v2 <- parseVecExp
  return (Disc (Custom v1 v2))

----------------------
-- Continuas
----------------------

parseVarCont :: P ExpAle
parseVarCont = try parseNorm <|> try parseUnif <|> parseExpo

parseNorm :: P ExpAle
parseNorm = do try (reserved "N") <|> reserved "Normal"
                parens $ do
                    m <- parseNumExp
                    reservedOp ","
                    u <- parseNumExp
                    return (Cont (Norm m u))

parseExpo :: P ExpAle
parseExpo = do try (reserved "Exp") <|> reserved "Exponencial"
               parens $ do
                  a <- parseNumExp
                  return (Cont (Expo a))

parseUnif :: P ExpAle
parseUnif = do try (reserved "Unif") <|> reserved "Uniforme"
               parens $ do
                  a <- parseNumExp
                  reservedOp ","
                  b <- parseNumExp
                  return (Cont (Unif a b))

----------------------
-- Funciones numéricas sobre aleatorias
----------------------

parseNumFunc :: P ExpNum
parseNumFunc = try parseEsp
           <|> try parseVari
           <|> try parseDesv
           <|> try parseFdp
           <|> try parseMaxP
           <|> try parseMinP
           <|> parseMaxFDP

parseEsp :: P ExpNum
parseEsp = do reserved "E"
              v <- parens parseAleExp
              return (Esp v)

parseVari :: P ExpNum
parseVari = do reserved "V"
               v <- parens parseAleExp
               return (Vari v)

parseDesv :: P ExpNum
parseDesv = do reserved "SD"
               v <- parens parseAleExp
               return (Desv v)

parseFdp :: P ExpNum
parseFdp = do reserved "fdp"
              parens $ do
                v <- parseAleExp
                reservedOp ","
                n <- parseNumExp
                return (FDP v n)

parseMaxP :: P ExpNum
parseMaxP = do reserved "maxP"
               v <- parens parseAleExp
               return (MaxP v)

parseMinP :: P ExpNum
parseMinP = do reserved "minP"
               v <- parens parseAleExp
               return (MinP v)

parseMaxFDP :: P ExpNum
parseMaxFDP = do reserved "maxFDP"
                 v <- parens parseAleExp
                 return (MaxFDP v)

----------------------
-- Funciones vectoriales
----------------------

parseModa :: P ExpVec
parseModa = do reserved "moda"
               v <- parens parseAleExp
               return (Moda v)

parseAModa :: P ExpVec
parseAModa = do reserved "antimoda"
                v <- parens parseAleExp
                return (AModa v)

----------------------
-- Declaraciones
----------------------

parseNumVDec :: P (Decl ExpNum)
parseNumVDec = do x <- lowerString
                  reservedOp "="
                  v <- parseNumExp
                  return (Decl { declName = x, declBody = v })

parseVarAleDec :: P (Decl ExpAle)
parseVarAleDec = do x <- upperString
                    reservedOp "~" <|> reservedOp ":="
                    v <- parseAleExp
                    return (Decl { declName = x, declBody = v })

parseVecDec :: P (Decl ExpVec)
parseVecDec = do x <- lowerString
                 reservedOp "="
                 v <- parseVecExp
                 return (Decl { declName = x, declBody = v })

----------------------
-- Wrapper opcional para programa
----------------------

data Stmt
  = NumStmt (Decl ExpNum)
  | VecStmt (Decl ExpVec)
  | AleStmt (Decl ExpAle)
  deriving (Show, Eq)

parseStmt :: P Stmt
parseStmt =
      try (AleStmt <$> parseVarAleDec)
  <|> try (VecStmt <$> parseVecDec)
  <|> (NumStmt <$> parseNumVDec)

parseProgram :: P [Stmt]
parseProgram = whiteSpace *> many parseStmt <* eof