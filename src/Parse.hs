{-
Module      : Parse
Description : Parser for the probabilistic language.
-}

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

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
reservedNamesList :: [String]
reservedNamesList =
  [ "Bin", "Binomial"
  , "Poi", "Poisson"
  , "Geo", "Geometric"
  , "BN", "Pascal"
  , "HG", "Hipergeometric"
  , "N", "Normal"
  , "Exp", "Exponential"
  , "Unif", "Uniform"
  , "P", "E", "SD", "V", "pdf", "F"
  , "mode", "maxP", "maxPDF"
  , "mk", "print", "table", "plot"
  , "ex", "stationary", "simulate", "start", "prob"
  ]

reservedOpNamesList :: [String]
reservedOpNamesList =
  [ "=", ",", "->", "<-", "~"
  , "<=", "<", ">=", ">", ":="
  , "+", "-", "*", "/"
  ]

lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef
  { commentStart   = "/*"
  , commentEnd     = "*/"
  , commentLine    = "//"
  , reservedNames  = reservedNamesList
  , reservedOpNames = reservedOpNamesList
  }

whiteSpaceNoNL :: P ()
whiteSpaceNoNL = skipMany (simpleSpace <|> lineComment <|> blockComment)
  where
    simpleSpace  = skipMany1 (oneOf " \t")
    lineComment  = try (string "//" *> many (noneOf "\n")) >> return ()
    blockComment = try (string "/*" *> manyTill anyChar (try (string "*/"))) >> return ()

whiteSpace :: P ()
whiteSpace = whiteSpaceNoNL

lexemeNoNL :: P a -> P a
lexemeNoNL p = p <* whiteSpaceNoNL

symbolNoNL :: String -> P String
symbolNoNL = lexemeNoNL . string

eol :: P ()
eol =
      try (string "\r\n" >> return ())
  <|> (string "\n" >> return ())
  <|> (string "\r" >> return ())

lineBreak :: P ()
lineBreak = try $ do
  whiteSpaceNoNL
  eol
  return ()

natural :: P Integer
natural = lexemeNoNL $ read <$> many1 digit

double :: P Double
double = lexemeNoNL $ try floatP <|> intP
  where
    intP = do
      ds <- many1 digit
      return (fromInteger (read ds :: Integer))
    floatP = do
      a <- many1 digit
      _ <- char '.'
      b <- many1 digit
      return (read (a ++ "." ++ b) :: Double)

parens :: P a -> P a
parens = between (symbolNoNL "(") (symbolNoNL ")")

brackets :: P a -> P a
brackets = between (symbolNoNL "[") (symbolNoNL "]")

braces :: P a -> P a
braces = between (symbolNoNL "{") (symbolNoNL "}")

identifier :: P String
identifier = lexemeNoNL $ try $ do
  c  <- letter <|> char '_'
  cs <- many (alphaNum <|> oneOf "_'")
  let s = c : cs
  if s `elem` reservedNamesList
    then unexpected ("reserved word " ++ show s)
    else return s

reserved :: String -> P ()
reserved w = lexemeNoNL $ try $ do
  _ <- string w
  notFollowedBy (alphaNum <|> oneOf "_'")
  return ()

reservedOp :: String -> P ()
reservedOp s = lexemeNoNL (try (string s) >> return ())


upperString :: P String
upperString = do
  s@(c:_) <- identifier
  if isUpper c
    then return s
    else unexpected "uppercase identifier was expected"

lowerString :: P String
lowerString = do
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

-- ====================================================
-- | Numeric                                              
-- =====================================================
parseConstNum :: P Exp                                  
parseConstNum = ConstN <$> double                       
                                                    
parseUMinus :: P Exp                                    
parseUMinus = do reservedOp "-"                         
                 UMinus <$> parseNumBaseAtom            
                                                    
parseAddOp :: P (Exp -> Exp -> Exp)                     
parseAddOp = (reservedOp "+"  >> return (OpNum Plus))   
         <|> (reservedOp "-" >> return (OpNum Minus))   
                                                    
parseMulOp :: P (Exp -> Exp -> Exp)                     
parseMulOp = (reservedOp "*" >> return (OpNum Times))   
         <|> (reservedOp "/" >> return (OpNum Div))     



-- =========================================================
-- | Vectors                                                  
-- =========================================================
parseVector :: P (Vec Exp)                                  
parseVector = V.fromList                                    
          <$> parens (parseNumExp `sepBy1` reservedOp ",")  
                                                            
parseConstVec :: P Exp                                      
parseConstVec = ConstV <$> parseVector                      
                                                            
parseAccess :: P Exp                                        
parseAccess = do v <- parseVecExp                           
                 n <- brackets parseNumExp                  
                 return (Access v n)                        


-- ===============================================
-- | Random variables                               
-- ===============================================
-- | Discrete                                       
parseRandDisc :: P RandExp                        
parseRandDisc = try parseBin                      
            <|> try parsePoiss                    
            <|> try parseGeo                      
            <|> try parsePasc                     
            <|> try parseHiper                    
            <|> parseCustom                       
                                                  
parseBin :: P RandExp                             
parseBin = do try (reserved "Bin")                
                <|> reserved "Binomial"           
              parens $ do                         
                n <- parseNumExp                  
                reservedOp ","                    
                p <- parseNumExp                  
                return (DiscE (BinE n p))         
                                                  
parsePoiss :: P RandExp                           
parsePoiss = do try (reserved "Poi")              
                  <|> reserved "Poisson"          
                parens $ do                       
                    n <- parseNumExp              
                    return (DiscE (PoissE n))     
                                                  
parseGeo :: P RandExp                             
parseGeo = do try (reserved "Geo")                
                <|> reserved "Geometric"         
              parens $ do                         
                  p <- parseNumExp                
                  return (DiscE (GeoE p))         
                                                  
parsePasc :: P RandExp                            
parsePasc = do try (reserved "BN")                
                <|> reserved "Pascal"             
               parens $ do                        
                  n <- parseNumExp                
                  reservedOp ","                  
                  p <- parseNumExp                
                  return (DiscE (PascE n p))      
                                                  
parseHiper :: P RandExp                           
parseHiper = do try (reserved "HG")               
                  <|> reserved "Hipergeometric"  
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
                  v2 <- parseVecExp               
                  return (DiscE (CustomE v1 v2))  
                                                  
--------------------------------------------------
-- | Continuous                                     
parseRandCont :: P RandExp                        
parseRandCont = try parseNorm                     
            <|> try parseUnif                     
            <|> parseExpo                         
                                                  
parseNorm :: P RandExp                            
parseNorm = do try (reserved "N")                 
                <|> reserved "Normal"             
               parens $ do                        
                 m <- parseNumExp                 
                 reservedOp ","                   
                 u <- parseNumExp                 
                 return (ContE (NormE m u))       
                                                  
parseExpo :: P RandExp                            
parseExpo = do try (reserved "Exp")               
                <|> reserved "Exponential"        
               parens $ do                        
                  a <- parseNumExp                
                  return (ContE (ExpoE a))        
                                                  
parseUnif :: P RandExp                            
parseUnif = do try (reserved "Unif")              
                <|> reserved "Uniform"           
               parens $ do                        
                  a <- parseNumExp                
                  reservedOp ","                  
                  b <- parseNumExp                
                  return (ContE (UnifE a b))      



-- ===================================================
-- | Markov Chains                                       
-- ===================================================
parseMarkov :: P MarkovExp                             
parseMarkov = do reserved "mk"                         
                 l <- parseNames                       
                 return (MarkovE l)                    
                                                       
                                                       
parseNextDist :: P Exp                                 
parseNextDist = do reserved "ex"                       
                   parens $ do                         
                    x <- parseMarkovExp                
                    reservedOp ","                     
                    n <- parseNumExp                   
                    return (NextDist x n)              
                                                       
parseStationary :: P Exp                               
parseStationary = do reserved "stationary"             
                     x <- parens parseMarkovExp        
                     return (Stationary x)             
-------------------------------------------------------
-- Nodes                                               
-------------------------------------------------------
parseNeight :: P (Name, Exp)                           
parseNeight = do parens $ do                           
                  n <- identifier                      
                  reservedOp ","                       
                  p <- parseNumExp                     
                  return (n, p)                        
                                                       
parseNode :: P NodeExp                                 
parseNode = do l <- brackets                           
                (parseNeight `sepBy1` reservedOp ",")  
               return (NE l)                           


-- ===========================================
-- | Stats                                   
-- ===========================================
-- | Numeric Stats                            
parseNumStat :: P Exp                       
parseNumStat = try parseMean                
           <|> try parseVariance            
           <|> try parseStdDev              
           <|> try parsePDF                 
           <|> try parseMaxP                
           <|> parseMaxPDF                  
                                        
--------------------------------------------
-- | Vector Stats                             
parseVecStat :: P Exp                       
parseVecStat = try parseSMode               
--------------------------------------------
                                        
parseMean :: P Exp                          
parseMean = do reserved "E"                 
               v <- parens parseRandExp     
               return (Mean v)              
                                        
parseVariance :: P Exp                      
parseVariance = do reserved "V"             
                   v <- parens parseRandExp 
                   return (Variance v)      
                                        
parseStdDev :: P Exp                        
parseStdDev = do reserved "SD"              
                 v <- parens parseRandExp   
                 return (StdDev v)          
                                        
parsePDF :: P Exp                           
parsePDF = do reserved "pdf"                
              parens $ do                   
                v <- parseRandExp           
                reservedOp ","              
                n <- parseNumExp            
                return (PDF v n)            
                                        
parseMaxP :: P Exp                          
parseMaxP = do reserved "maxP"              
               v <- parens parseRandExp     
               return (MaxP v)              
                                        
parseMaxPDF :: P Exp                        
parseMaxPDF = do reserved "maxPDF"          
                 v <- parens parseRandExp   
                 return (MaxPDF v)          
                                        
parseSMode :: P Exp                         
parseSMode = do reserved "mode"             
                v <- parens parseRandExp    
                return (Mode v)             


-- ========================================================
-- | Path                                                     
-- ========================================================
parsePath :: P Path                                         
parsePath = V.fromList                                      
          <$> brackets (identifier `sepBy1` reservedOp ",") 
                                                            
parseNames :: P Path                                        
parseNames = V.fromList                                     
          <$> parens (identifier `sepBy1` reservedOp ",")   


-- =================================================
-- | Probabilitys                                     
-- =================================================
prob :: P Exp                                       
prob = try probRand <|> probMk                      
----------------------------------------------------
--  | Random Variables Probabilitys                   
probRand :: P Exp                                   
probRand = do reserved "P"                          
              parens parseProbRand                  
                                                    
parseProbRand :: P Exp                              
parseProbRand = try parsePOpBt <|> parsePOp         
                                                    
parsePOp :: P Exp                                   
parsePOp = try (do n <- parseNumExp                 
                   op <- parseOpposite              
                   v <- parseRandExp                
                   return (Prob v op n))            
      <|> do v <- parseRandExp                      
             op <- parseOp                          
             n <- parseNumExp                       
             return (Prob v op n)                   
                                                    
parsePOpBt :: P Exp                                 
parsePOpBt = do n <- parseNumExp                    
                op1 <- parseOpposite                
                v <- parseRandExp                   
                op2 <- parseOp                      
                m <- parseNumExp                    
                return (ProbBetween v op1 n op2 m)  
                                                    
----------------------------------------------------
-- | Operations Symbols                               
parseOp :: P OpComp                                 
parseOp = try (reservedOp "<=" >> return Lte)       
      <|> try (reservedOp "<"  >> return Lt)        
      <|> try (reservedOp ">=" >> return Gte)       
      <|> try (reservedOp ">"  >> return Gt)        
      <|> try (reservedOp "/=" >> return NEq)       
      <|> (reservedOp "=" >> return Eq)             
                                                    
parseOpposite :: P OpComp                           
parseOpposite = try (reservedOp "<=" >> return Gte) 
      <|> try (reservedOp "<"  >> return Gt)        
      <|> try (reservedOp ">=" >> return Lte)       
      <|> try (reservedOp ">"  >> return Lt)        
      <|> try (reservedOp "/=" >> return NEq)       
      <|> (reservedOp "=" >> return Eq)             
                                                    
----------------------------------------------------
-- | Markov Probabilitys                              
probMk :: P Exp                                     
probMk = do reserved "F"                            
            parseProbMk                             
                                                    
parseProbMk :: P Exp                                
parseProbMk = try parseProbStep <|>                 
              try parseProbPath <|>                 
              parseProbHit                          
                                                    
parseProbStep :: P Exp
parseProbStep = do n <- parseNumExp
                   parens $ do
                     x <- parseMarkovExp
                     reservedOp ","
                     i <- identifier
                     reservedOp ","
                     j <- identifier
                     return (ProbStep x i j n)   
                                                    
parseProbPath :: P Exp                              
parseProbPath = do parens $ do                      
                    x <- parseMarkovExp             
                    reservedOp ","                  
                    c <- parsePathExp               
                    return (ProbPath x c)           
                                                    
parseProbHit :: P Exp
parseProbHit = do parens $ do
                    x <- parseMarkovExp
                    reservedOp ","
                    i <- identifier
                    reservedOp ","
                    j <- identifier
                    return (ProbHit x i j)     


-- ======================================================
-- Simulations                                        
-- ======================================================
parseSimulateName :: P Exp                            
parseSimulateName = do reserved "simulate"            
                       parens $ do                    
                        x <- parseMarkovExp           
                        reservedOp ","                
                        n <- parseNumExp              
                        reservedOp ","                
                        reserved "start"              
                        reservedOp "="                
                        m <- identifier               
                        return (SimulFromName x m n)  
                                                      
parseSimulateVec :: P Exp                             
parseSimulateVec = do reserved "simulate"             
                      parens $ do                     
                        x <- parseMarkovExp           
                        reservedOp ","                
                        n <- parseNumExp              
                        reservedOp ","                
                        reserved "prob"               
                        reservedOp "="                
                        v <- parseVecExp              
                        return (SimulFromVec x v n)   


-- ===============================================
-- | Expressions                                            
-- ===============================================
parseExp :: P Exp                                
parseExp =                                       
      try parseNumExp                            
  <|> try parseVecExp                            
  <|> try parseRandExp                           
  <|> try parseMarkovExp                         
  <|> parsePathExp                               
                                            
-------------------------------------------------
-- | Numeric                                       
parseNumBaseAtom :: P Exp                        
parseNumBaseAtom =                               
      try prob                                   
  <|> try parseAccess                            
  <|> try parseNumStat                           
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
                                            
-------------------------------------------------
-- | Vectors                                       
parseVecExp :: P Exp                             
parseVecExp = try parseConstVec                  
          <|> try parseVarL                      
          <|> parseVecStat                       
                                            
-------------------------------------------------
-- | Random                                        
parseRandExp :: P Exp                            
parseRandExp = try                               
     parseVarU                                   
 <|> try (Rand <$> parseRandDisc)                
 <|> (Rand <$> parseRandCont)                    
                                            
-------------------------------------------------
-- | Path                                          
parsePathExp :: P Exp                            
parsePathExp = try (ConstP <$> parsePath)       
            <|> try parseVarL                    
            <|> try parseSimulateName            
            <|> parseSimulateVec                 
                                            
-------------------------------------------------
-- | Markov                                        
parseMarkovExp :: P Exp                          
parseMarkovExp = try (Markov <$> parseMarkov)    
              <|> try parseVarU                  
              <|> try parseNextDist             
              <|> parseStationary               



-- ====================================================== 
-- | Commands                                               
-- ====================================================== 
parseComm :: P Comm                                     
parseComm =  parseDec                                   
          <|> try parsePrintNum                         
          <|> try parseTable                            
          <|> parsePlot                                 
                                                        
------------------------------------------------------- 
-- | Declarations                                         
parseDec :: P Comm                                      
parseDec = try parseDecNode                             
       <|> try parseDecNum                              
       <|> try parseDecAle                              
       <|> try parseDecVec                              
       <|> try parseDecPath                             
       <|> parseDecMk                                   
                                                        
parseDecNum :: P Comm                                   
parseDecNum = do x <- lowerString                       
                 reservedOp "="                         
                 e <- parseNumExp                       
                 return (Let x e)                       
                                                        
parseDecAle :: P Comm                                   
parseDecAle = do x <- upperString                       
                 reservedOp "~" <|> reservedOp ":="     
                 e <- parseRandExp                      
                 return (Let x e)                       
                                                        
parseDecVec :: P Comm                                   
parseDecVec = do x <- lowerString                       
                 reservedOp "="                         
                 e <- parseVecExp                       
                 return (Let x e)                       
                                                        
parseDecMk :: P Comm                                    
parseDecMk = do x <- upperString                        
                reservedOp "="                          
                e <- parseMarkovExp                     
                return (Let x e)                        
                                                        
parseDecPath :: P Comm                                  
parseDecPath = do x <- lowerString                      
                  reservedOp "="                        
                  e <- parsePathExp                     
                  return (Let x e)                      
                                                        
parseDecNode :: P Comm                                  
parseDecNode = do x <- identifier                       
                  reservedOp "->"                       
                  e <- parseNode                        
                  return (LetN x e)                     
                                                        
------------------------------------------------------- 
-- | Print                                                
parsePrintNum :: P Comm                                 
parsePrintNum = do reserved "print"                     
                   e <- parens parseExp                 
                   return(Print e)                      
                                                        
------------------------------------------------------- 
-- | Table                                                
parseTable :: P Comm                                    
parseTable = do reserved "table"                        
                parens $ do                             
                  x <- parseRandExp                     
                  (do reservedOp ","                    
                      n <- parseNumExp                  
                      reservedOp ","                    
                      m <- parseNumExp                  
                      return (TableR x n m))            
                   <|> return (Table x)                 
                                                        
------------------------------------------------------- 
-- | Plot                                                 
parsePlot :: P Comm
parsePlot = do reserved "plot"
               x <- parens (try parseRandExp <|> parseMarkovExp)
               return (Plot x)


-- ==================================================
-- Main
-- ==================================================
-- | Program parser
program :: P [Comm]
program = do
  skipMany lineBreak
  xs <- sepEndBy parseComm (many1 lineBreak >> return ())
  whiteSpaceNoNL
  eof
  return xs

runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpaceNoNL *> p <* eof) () filename s

parseProgram :: P [Comm]
parseProgram = program