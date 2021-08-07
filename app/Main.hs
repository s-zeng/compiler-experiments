{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative hiding (Const)
import Data.Function
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Zl.AST.Syntax
import Zl.Parser.Core

plus, minus, times, divide :: Parser Op
plus = char '+' $> Plus
minus = char '-' $> Minus
times = char '*' $> Times
divide = char '/' $> Divide

addOp, mulOp :: Parser Op
addOp = plus <|> minus
mulOp = times <|> divide

chainOp :: Parser Expr -> Parser Op -> Parser Expr
chainOp arg op = do
  first_arg <- arg
  rest <- many $ do
    o <- op
    a <- arg
    pure $ (flip $ BinOp o) a
  pure $ foldl' (&) first_arg rest

_token :: Parser String
_token = do
  x <- alphabetic <|> char '_'
  rest <- many $ alphabetic <|> digit <|> char '_'
  pure $ x : rest

token :: Parser Expr
token = Const . Token <$> _token

number :: Parser Expr
number = Lit . PFloat <$> (floating <|> fromInteger <$> integer)

term, factor, expr :: Parser Expr
term = number <|> token <|> parens expr
factor = chainOp term mulOp
expr = chainOp factor addOp

-- ========================================================================== --

data Statement = Set String Expr deriving (Show, Eq, Ord)

statement :: Parser Statement
statement = do
  var <- _token
  char '='
  val <- expr
  pure $ Set var val

-- ========================================================================== --

data Exception = SyntaxError String | DivideByZero | Undefined String deriving (Show, Eq, Ord)

type Result = Either Exception

processParse :: [(a, String)] -> Result a
processParse [] = Left $ SyntaxError "Bad input!"
processParse ((e, s) : xs) =
  if null s
    then Right e
    else processParse xs <> Left (SyntaxError s)

-- ========================================================================== --
-- ========================================================================== --

type HaskellOp = Result Double -> Result Double -> Result Double

type SymbolTable = Map String Double

opToFunc :: Op -> HaskellOp
opToFunc Plus = liftA2 (+)
opToFunc Times = liftA2 (*)
opToFunc Minus = liftA2 (flip subtract)
opToFunc Divide = \x y -> do
  a <- x
  b <- y
  if b == 0
    then Left DivideByZero
    else pure (a / b)
opToFunc _ = error "incomlete: opToFunc"

interpExpr :: SymbolTable -> Expr -> Result Double
interpExpr _ (Lit (PFloat x)) = Right x
interpExpr table (Const (Token s)) = case Map.lookup s table of
  Nothing -> Left (Undefined s)
  (Just x) -> Right x
interpExpr table (BinOp op x y) = opToFunc op (interpExpr table x) (interpExpr table y)
interpExpr _ _ = error "incomplete: interpExpr"

-- ========================================================================== --

interpStatement :: SymbolTable -> Statement -> Result SymbolTable
interpStatement table (Set x e) = do
  val <- interpExpr table e
  pure $ Map.insert x val table

-- ========================================================================== --

data InputLine = Calculation Expr | State Statement deriving (Show, Eq, Ord)

parseInput :: String -> Result InputLine
parseInput = processParse . runParse input_line
  where
    input_line = State <$> statement <|> Calculation <$> expr

newTable :: SymbolTable -> InputLine -> Result SymbolTable
newTable table (State stmnt) = interpStatement table stmnt
newTable table _ = pure table

calculate :: SymbolTable -> InputLine -> Result String
calculate table (Calculation e) = show <$> interpExpr table e
calculate table (State (Set v r)) = ((v <> " = ") <>) . show <$> interpExpr table r

tupleSeq :: (Either a b, Either a c) -> Either a (b, c)
tupleSeq (Right a, Right b) = Right (a, b)
tupleSeq (Left a, _) = Left a
tupleSeq (_, Left b) = Left b

_loop :: SymbolTable -> String -> Either Exception (SymbolTable, String)
_loop table line = do
  val <- parseInput line
  tupleSeq (newTable table val, calculate table val)

loop :: SymbolTable -> IO ()
loop table = do
  putStr ">>= "
  hFlush stdout
  x <- filter (not . flip elem " \t\r\n") <$> getLine
  let new_vals = _loop table x
  case new_vals of
    (Left e) -> print e >> loop table
    (Right (new_vartable, val)) -> putStrLn val >> loop new_vartable

main :: IO ()
main = loop Map.empty
