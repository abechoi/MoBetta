module MoBettaEngine where

-- Abe Choi
-- CPSC-354-01

import System.IO
import qualified Data.HashMap as HM-- easy lookup and update of variables
import Control.Monad.State
import Control.Applicative
import Data.Maybe (fromMaybe) -- using fromMaybe to simplify some code

import MoBettaAST

type Env = HM.Map String Integer

emptyEnv :: Env
emptyEnv = HM.fromList []

type Computation t = StateT Env IO t
type Action = Computation ()
type IntCalc = Computation Integer
type BoolCalc = Computation Bool

statementAction :: Statement -> Action
statementAction (Print e) = printAction (intCalc e)
statementAction (Msg s) = msgAction s
statementAction (Read v) =  readAction v
statementAction (If b s1 s2) =
  ifAction (boolCalc b) (statementAction s1) (statementAction s2)
statementAction (While b s) = whileAction (boolCalc b) (statementAction s)
statementAction (Assign v e) = assignAction v (intCalc e)
statementAction (Block ls) = makeProgram ls
                        
makeProgram ls = blockAction $ map statementAction ls

doIO :: IO a -> Computation a
doIO = lift

updateEnv :: String -> Integer -> Computation ()
updateEnv name val = modify $ HM.insert name val

retrieveEnv :: String -> IntCalc
retrieveEnv name = do
  val <- gets $ HM.lookup name
  return $ fromMaybe (varNotFound name) val
  where
    varNotFound name = error $ "Identifier \"" ++ name ++ "\" not defined."

readAction :: String -> Action
readAction v = do
  x <- doIO getInt
  updateEnv v x
  where
    getInt = do
      inp <- getLine
      return $ read inp

msgAction :: String -> Action
msgAction s = doIO $ putStr s

printAction :: IntCalc -> Action
printAction calc = do
  n <- calc
  doIO $ putStr (show n)

assignAction :: String -> IntCalc -> Action
assignAction v intCalc = do
  value <- intCalc
  updateEnv v value

ifAction :: BoolCalc -> Action -> Action -> Action
ifAction boolCalc action1 action2 = do
  condition <- boolCalc
  if condition
    then action1
    else action2

whileAction :: BoolCalc -> Action -> Action
whileAction boolCalc action = do
  condition <- boolCalc
  when
    condition whileLoop
  where
    whileLoop = do
      action
      whileAction boolCalc action

blockAction :: [Action] -> Action
blockAction [] = return ()
blockAction (a:ls) = do
  a
  blockAction (ls)

cOpTable = [ (Less, (<))
            , (LessEqual, (<=))
            , (Greater, (>))
            , (GreaterEqual, (>=))
            , (Equal, (==))
            , (NEqual, (/=)) ]

bOpTable = [ (And, (&&))
            , (Or, (||)) ]
unOpTable = [ (Not, (not)) ]

boolCalc :: BExpr -> BoolCalc
boolCalc (BoolConst b) = return b
boolCalc (Reln cOp expr1 expr2) = do
  n1 <- (intCalc expr1)
  n2 <- (intCalc expr2)
  let (Just comp) = (lookup cOp cOpTable)
  return (comp n1 n2)
boolCalc (BBin op expr1 expr2) = do
  n1 <- (boolCalc expr1)
  n2 <- (boolCalc expr2)
  let (Just bbin) = (lookup op bOpTable)
  return (bbin n1 n2)
boolCalc (BUn op expr) = do
  n <- (boolCalc expr)
  let (Just bun) = (lookup op unOpTable)
  return (bun n)

opTable = [(Mul, (*))
          , (Div, div)
          , (Mod, mod)
          , (Add, (+))
          , (Sub, (-)) ]



intCalc :: AExpr -> IntCalc
intCalc (Var v) = retrieveEnv v
intCalc (IntConst val) = return val
intCalc (ABin op expr1 expr2) = do
  n1 <- (intCalc expr1)
  n2 <- (intCalc expr2)
  let (Just func) = (lookup op opTable)
  return (func n1 n2)
intCalc (AUn op expr) = do
  n <- (intCalc expr)
  return (negate n)