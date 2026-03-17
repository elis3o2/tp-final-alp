module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Char (isSpace)
import AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-----------------------
-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = [ "B", "Bin","Binomial",
                          "Po", "Poisson",
                          "G", "Geo", "Geometrica",
                          "Pa", "Pasc", "Pascal",
                          "H", "Hiper", "Hipergeometrica",
                          "N", "Normal"]
    , reservedOpNames = ["=", "(", ")", ",", "->", "<-", "~", "<=", "<", ">=", ">"]
    , whiteSpace      = spaceNoNL
    }
  )


-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

parseTermSymbol :: Parser (Exp Num -> Exp Num -> Exp Num)
parseTermSymbol = do reservedOp lis "+"
                     return Plus
                  <|> reservedOp lis "-"
                      return Minus 

parseFactSymbol :: Parser (Exp Num -> Exp Num -> Exp Num)
parseFactSymbol = do reservedOp lis "*"
                     return Times
                  <|> reservedOp lis "/"
                      return Div 


-- parseVar se construye un parser
-- con la clasificacion de palabras clave
parseVar :: Parser (Exp Num)
parseVar =
  do v <- identifier lis
     return (Var v)

-- Parser de números enteros negativos
parseUMinus :: Parser (Exp Num)
parseUMinus = do reservedOp lis "-"
                  e <- parseNat
                    <|> parseVar
                    <|> parens lis intexp
                  return (UMinus e)



-- Parser de números naturales
parseNat :: Parser (Exp Num)
parseNat = do n <- (natural lis)
              return (Const (fromIntegral n))

atom :: Parser (Exp Num)
atom = parseUMinus   
       <|> parseNat       
       <|> parens lis intexp


parseTerm :: Parser (Exp Num)
parseTerm = chainl1 atom parseFactSymbol

intexp :: Parser (Exp Num)
intexp = chainl1 parseTerm parseTermSymbol


-----------------------------------
--- Parser de funciones sobre Variables Aleatorias
-----------------------------------


parseProb :: Parser (Exp Num)
parseProb = do reserved lis "P"
               parens lis parseRel
  

parseRel :: Parser (Exp Num)
parseRel = try parseBetween <|> try parsePOp


parsePOp :: Parser (Exp Num)
parsePOp = do n <- parseNum
              op <- parseOp
              v <- parseVarDisc
              return (POp v op n) 
            <|> do v <- parseVarDisc
                   op <- parseOp
                   n <- parseNum
                   return (POp v op n)



parseBetween :: Parser (Exp Num)
parseBetween = do n <- parseNum
                  op1 <- parseOp
                  v <- parseVarDisc
                  op2 <- parseOp
                  m <- parseNum
                  return (PBetween v op1 n op2 m)


parseOp :: Parser Op
parseOp = do  symbol lis "<="
              return Lte
          <|> symbol lis "<"  
              return Lt
          <|> symbol lis ">="
              return Gte
          <|> symbol lis ">" 
              return Gt
          <|> symbol lis "="
              return Eq
          <|> symbol lis "/=" 
              return NEq

parseEsp :: Parser (Exp Num)
parseEsp = do reserved lis "E"
               parens lis
                v <- parseVarDisc
                return (Esp v)
  

parseVar :: Parser (Exp Num)
parseVar = do reserved lis "V"
              parens lis
               v <- parseVarDisc
               return (Var v)


parseDesv :: Parser (Exp Num)
parseDesv = do reserved lis "D"
               parens lis
               v <- parseVarDisc
               return (Desv v)



parseMax :: Parser (Exp LNum)
parseMax = do reserved lis "max" 
              parens lis
              v <- parseVarDisc
              return (MaxP v)


parseMin :: Parser (Exp LNum)
parseMin = do reserved lis "min"
              parens lis
              v <- parseVarDisc
              return (MinP v)


parseMost :: Parser (Exp Num)
parseMost = do reserved lis "most"
                parens lis
                v <- parseVarDisc
                return (MostP v)

parseLeast :: Parser (Exp Num)
parseLeast = do reserved lis "least"
                  parens lis
                  v <- parseVarDisc
                  return (LeastP v)

-----------------------
-- Parsers básicos

parseName :: Parser Name
parseName = some (satisfy (not . isSpace))

parseExp :: Parser Exp
parseExp

parseVarAleName :: Parser VarAle


parseVarNum :: Parser Num

--------------------
-- Parser de Variables Aleatorias Discretas
---------------------

parseVarDisc :: Parser VarDisc
parseVarDisc = do parseVarAleName <|> parseBin <|> parsePoiss <|> parseGeo <|> parsePasc <|> parseHiper <|> parseT

            
parseBin :: Parser Bin
parseBin = do reserved lis "B" <|> reserved lis "Bin" <|> reserved lis "Binomial"
              parseBin'

parseBin' :: Parser Bin
parseBin' = do parens lis 
               n <- parseNum
               symbol lis ","
               p <- parseNum
               return (Bin n p)

parsePoiss :: Parser Poiss
parsePoiss = do reserved lis "Poiss" <|> reserved lis "Poisson"
                parsePoiss'

parsePoiss' :: Parser Poiss
parsePoiss' = do parens lis
                 n <- parseNum
                 return (Poiss n)


parseGeo :: Parser Geo
parseGeo = do reserved lis "G" <|> reserved lis "Geometrica"
              parseGeo'

parseGeo' :: Parser Geo
parseGeo' = do  parens lis
                p <- parseNum
                return (Geo p)

parsePasc :: Parser Pasc
parsePasc = do reserved lis "Pa" <|> reserved lis "Pascal"
               parsePasc'

parsePasc' :: Parser Pasc
parsePasc' = do parens lis
                n <- parseNum
                symbol lis ","
                p <- parseNum
                return (Pasc n p)


parseHiper :: Parser Hiper
parseHiper = do reserved lis "H" <|> reserved lis "Hipergeometrica"
                parseHiper'

parseHiper' :: Parser Hiper
parseHiper' = do parens lis
                 n <- parseNum
                 symbol lis ","
                 m <- parseNum
                 symbol lis ","
                 l <- parseNum
                 return (Hiper n m l)


parseT :: Parser T 
parseT = do reserved lis "["
            v1 <- parseList
            symbol lis ","
            v2 <- parseList
            symbol lis "]"
            return (T v1 v2)
  
--------------------
-- Parser de Variables Aleatorias Continuas
---------------------

parseVarCont :: Parser VarCont
parseVarCont = do parseNorm <|> parseUnif <|> parseExpo

parseNorm :: Parser Norm
parseNorm' = do reserved lis "N" <|> reserved lis "Normal"
                parseNorm'

parseNorm' :: Parser Norm
parseNorm' = do  parens lis
                 m <- parseNum
                 symbol lis ","
                 u <- parseNum
                 return (Norm m u)


parseExpo  :: Parser Expo
parseExpo = do reserved lis "Exp" <|> reserved lis "Exponencial"
               parseExpo'

parseExpo' :: Parser Expo
parseExpo' = do parens lis
                a <- parseNum
                return (Expo a)


parseUnif :: Parser Unif
parseUnif = do reserved lis "Unif" <|> reserved lis "Uniforme"
               parseUnif'

parseUnif' :: Parser Unif
parseUnif' = do parens lis
                a <- parseNum
                symbol lis ","
                b <- parseNum
                return (Unif a b)



--------------------
-- Parser de Vectores
---------------------

parseVector :: Parser (Exp Vector)
parseVector = do parens lis
                  vs <- many parseNum
                  return (Vector vs)



parseAcces :: Parser (Exp Num)
parseAcces = do l <- parseVector
                symbol lis "["
                e <- intexp
                symbol lis "]"
                return (Acces l e)


parse :: Parser (Decl Expr)
parse = do v  <- parseName
           reservedOp lis "="
           ex <- parseBin
           return (Decl v ex)