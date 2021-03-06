module MoBettaParser where

-- Abe Choi
-- CPSC-354-01
-- Parser for MoBetta.

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Data.Void

import MoBettaAST

type Parser = Parsec Void String

programParser = do
  spaceConsumer
  sepEndBy1 statementParser semicolon <?>  "program"


statementParser = choice
  [   skipStmt
    , printStmt
    , messageStmt
    , readStmt
    , ifStmt
    , whileStmt
    , assignmentStmt
    , blockStmt
  ] <?> "statement"
  where
    skipStmt = lexeme (string "skip") >> return Skip
    printStmt = do
      lexeme (string "print")
      e <- aExpr
      return (Print e)
    readStmt = do
      lexeme (string "read")
      i <- identifier
      return (Read i)
    messageStmt = do
      lexeme (string "message")
      s <- lexeme stringLiteral
      return (Msg s)
    ifStmt = do
      lexeme (string "if")
      b <- bExpr
      lexeme (string "then")
      t <- statementParser
      lexeme (string "else")
      e <- statementParser
      return (If b t e)
    whileStmt = do
      lexeme (string "while")
      b <- bExpr
      lexeme (string "do")
      d <- statementParser
      return (While b d)
    assignmentStmt = do
      vari <- identifier
      lexeme (char '=')
      e <- aExpr
      return (Assign vari e)
    blockStmt = do
      b <- between lbrace rbrace programParser
      return (Block b)

aExpr = makeExprParser aFactor aOpTable <?> "arithmetic expression"

aFactor = choice [ intConst
                , identifierExpr
                , between lparen rparen aExpr
                ] <?> "arithmetic factor"

aOpTable = [ [ prefix  "-"  (AUn Neg)
            , prefix  "+" id ]
          , [ binary  "*"  (ABin Mul)
            , binary  "/"  (ABin Div)
            , binary  "%"  (ABin Mod)]
          , [ binary  "+"  (ABin Add)
            , binary  "-"  (ABin Sub)  ] ]

bExpr :: Parser BExpr
bExpr = makeExprParser bFactor bOpTable

bFactor = choice [boolConst
                , comparison
                , between lparen rparen bExpr
                ] <?> "boolean factor"

bOpTable = [[binary "and" (BBin And), binary "or" (BBin Or)],[ prefix "not" (BUn Not)]]

comparison = do
    e1 <- aExpr
    c  <- comparator
    e2 <- aExpr
    return (Reln c e1 e2)

comparator = choice compTable <?> "comparator"

compTable = [
    atomic "<"  Less
  , atomic "<=" LessEqual
  , atomic ">"  Greater
  , atomic ">=" GreaterEqual
  , atomic "==" Equal
  , atomic "!=" NEqual
  ]

binary  opName f = InfixL (atomic opName f) 
prefix  opName f = Prefix (atomic opName f)
atomic  opName f = f <$ lexeme (string opName)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lparen = lexeme (char '(')
rparen = lexeme (char ')')
semicolon = lexeme (char ';')
lbrace = lexeme (char '{')
rbrace = lexeme (char '}')


identifier :: Parser String
identifier = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

identifierExpr = Var <$> identifier

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

intConst :: Parser AExpr
intConst = fmap IntConst intConst'
  where
    intConst' = (lexeme . try) ic
    ic = do
          x <- L.decimal
          notFollowedBy letterChar
          return x

boolConst :: Parser BExpr
boolConst = do
  b <- bc
  return(BoolConst b)
  where
    bc = choice [true, false]


true :: Parser Bool
true = lexeme (string "true") >> return True

false :: Parser Bool
false = lexeme (string "false") >> return False


tryit p = parse p "(--)"

mbparse = parse programParser
