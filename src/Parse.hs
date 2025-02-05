{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
ç
-}

module Parse (runP, P, program, expr) where

import Prelude hiding ( const )
import Lang
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Data.Char
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)
type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         commentLine    = "%",
         --reservedNames = ["="],
         reservedOpNames = ["?-",":-",",",";",".","=","\\=","is",
                            "+","-","*",">", ">=","print", "\\+",
                            "=:="]
        }

-- A utility parser that skips whitespace around a token
lexeme :: P a -> P a
lexeme p = p <* whiteSpace

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

integer :: P Integer
integer = Tok.integer lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

-- A variable is a word that starts with an uppercase letter or an underscore
variableP :: P String
variableP = try $ do
  firstChar <- satisfy (\c -> isUpper c || c == '_')       -- First character must be upper or underscore
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))  -- The rest can be any alphanumeric character
  return (firstChar : rest)

-- An atom A string of characters made up of upper-case letters, lower-case letters, digits, and the underscore character,
--   that begins with a lower-case letter.
atomString :: P String
atomString = try $ do
  firstChar <- satisfy isLower                             -- First character must be lowercase
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))  -- The rest can be any alphanumeric character
  return (firstChar : rest)

atomQuotes :: P String
atomQuotes = try $ do
  char '\''
  content <- many (noneOf "'")
  char '\''
  return content

reserved :: String -> P ()
reserved = Tok.reserved lexer 

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

surroundingSpaces :: P a -> P a
surroundingSpaces p = do 
  spaces
  res <- p
  spaces
  return res

num :: P Int
num = fromInteger <$> integer

var :: P Variable
var = variableP

atom :: P Atom
atom = atomString <|> atomQuotes

const :: P Const
const = CInt <$> num

-----------------------
-- Term Parsers
-----------------------

varTerm :: P Term
varTerm = TVar <$> var

atomTerm :: P Term
atomTerm = TAtom <$> atom

constTerm :: P Term
constTerm = TConst <$> const

complexTermArgs :: P [Term]
complexTermArgs = parens $ sepBy term sep where 
    sep = reservedOp "," -- >> try whiteSpace

complexTerm :: P Term
complexTerm = do
  a    <- atom
  args <- complexTermArgs
  return $ CTerm a args

-- BuiltIn Sugar Parsing
cterm1 :: String -> Term -> Term
cterm1 s t = CTerm s [t]
cterm2 :: String -> Term -> Term -> Term
cterm2 s t1 t2 = CTerm s [t1, t2]

binarySugar :: String -> Assoc -> Operator String () Identity Term
binarySugar s = Ex.Infix (reservedOp s >> return (cterm2 s))

prefix :: String -> Operator String () Identity Term
prefix s = Ex.Prefix (reservedOp s >> return (cterm1 s))

tableSugar :: [[Operator String () Identity Term]]
tableSugar = [[binarySugar "*" Ex.AssocLeft,
               binarySugar "+" Ex.AssocLeft,
               binarySugar "-" Ex.AssocLeft],
              [binarySugar "=" Ex.AssocLeft,
               binarySugar "\\=" Ex.AssocLeft,
               binarySugar "=:=" Ex.AssocLeft,
               binarySugar "is" Ex.AssocLeft,
               binarySugar ">" Ex.AssocLeft,
               binarySugar ">=" Ex.AssocLeft,
               binarySugar "<" Ex.AssocLeft],
              [prefix "\\+"]
             ]
              --[binarySugar "is" Ex.AssocLeft]]

builtIn :: P Term
builtIn = Ex.buildExpressionParser tableSugar term' <?> "BuiltIn"

term' :: P Term
term' = surroundingSpaces (try (parens term) <|> try complexTerm <|> varTerm <|> constTerm <|> atomTerm) <?> "term"

term :: P Term
term = builtIn <|> complexTerm <|> varTerm <|> constTerm <|> atomTerm <?> "term"

-----------------------
-- Term Operation Tree Parser
-----------------------

binaryT :: String -> BoolOp -> Assoc -> Operator String () Identity TermOpTree
binaryT s op = Ex.Infix (reservedOp s >> return (Node op)) 

tableT :: [[Operator String () Identity TermOpTree]]
tableT = [[binaryT "," And Ex.AssocLeft],
          [binaryT ";" Or  Ex.AssocLeft]]

treeNode :: P TermOpTree
treeNode = Ex.buildExpressionParser tableT treeParser

leafParser :: P TermOpTree
leafParser = Leaf <$> term

treeParser :: P TermOpTree
treeParser = leafParser <|> parens treeNode <?> "Term Operation Tree"

treeParserAux :: P TermOpTree
treeParserAux = treeNode <|> leafParser <?> "Term Operation Tree"

-----------------------
-- Expresion Parser
----------------------

fact :: P Expr
fact = Fact <$> term <?> "fact"

rule :: P Expr
rule = do
    headTerm <- term
    reservedOp ":-"
    body <- treeParser
    return $ Rule headTerm body

ruleOrFact :: P Expr
ruleOrFact = do
    headTerm <- term  
    (do
        try whiteSpace
        reservedOp ":-"
        body <- treeParserAux  
        return $ Rule headTerm body
      )
      <|> return (Fact headTerm)

query :: P Expr
query = do
    reservedOp "?-"
    try whiteSpace
    body <- treeParserAux
    return $ Query body

expr :: P Expr
expr = do
  e <- query <|> ruleOrFact <?> "expr"
  reservedOp "."
  return e

-- | Parser de programas
program :: P Module
program = many expr 

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s