{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Sy.Parser.Core

import Control.Applicative
import Data.Function
import Data.Map (Map)
import Data.List
import System.IO
import qualified Data.Map as Map

data Op = Plus | Times | Minus | Divide deriving (Show, Eq, Ord, Enum)

data Expr = Number Double | Var String | BinOp Op Expr Expr deriving (Eq, Ord)

instance Show Expr where
    show (Number x)     = show x
    show (Var x)        = x
    show (BinOp op a b) = "(" <> show op <> " " <> show a <> " " <> show b <> ")"

plus, minus, times, divide :: Parser Op
plus   = char '+' *> pure Plus
minus  = char '-' *> pure Minus
times  = char '*' *> pure Times
divide = char '/' *> pure Divide

addOp, mulOp :: Parser Op
addOp = plus  <|> minus
mulOp = times <|> divide

chainOp :: Parser Expr -> Parser Op -> Parser Expr
chainOp arg op = do
    first_arg <- arg
    rest <- many $ do o <- op
                      a <- arg
                      pure $ (flip $ BinOp o) a 
    pure $ foldl' (&) first_arg rest

_token :: Parser String
_token = do
    x <- alphabetic <|> char '_'
    rest <- many $ alphabetic <|> digit <|> char '_'
    pure $ x:rest
token :: Parser Expr
token = Var <$> _token

number :: Parser Expr
number = Number <$> (floating <|> fromInteger <$> integer)

term, factor, expr :: Parser Expr
term   = number <|> token <|> parens expr
factor = chainOp term   mulOp
expr   = chainOp factor addOp

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

process_parse :: [(a, String)] -> Result a
process_parse [] = Left $ SyntaxError "Bad input!"
process_parse ((e, s):xs) = if null s
                               then Right e
                               else process_parse xs <> Left (SyntaxError s)

-- ========================================================================== --
-- ========================================================================== --

type HaskellOp = Result Double -> Result Double -> Result Double
type SymbolTable = Map String Double

opToFunc :: Op -> HaskellOp
opToFunc Plus   = liftA2 (+)
opToFunc Times  = liftA2 (*)
opToFunc Minus  = liftA2 (flip subtract)
opToFunc Divide = \x y -> do
    a <- x
    b <- y
    if (b == 0) then Left DivideByZero
                else pure (a/b)

interpExpr :: SymbolTable -> Expr -> Result Double
interpExpr _  (Number x) = Right x
interpExpr table (Var s) = case Map.lookup s table of
                             Nothing -> Left (Undefined s)
                             (Just x) -> Right x
interpExpr table (BinOp op x y) = opToFunc op (interpExpr table x) (interpExpr table y)

-- ========================================================================== --

interpStatement :: SymbolTable -> Statement -> Result SymbolTable
interpStatement table (Set x e) = do
    val <- interpExpr table e
    pure $ Map.insert x val table

-- ========================================================================== --

data InputLine = Calculation Expr | State Statement deriving (Show, Eq, Ord)

parse_input :: String -> Result InputLine
parse_input = process_parse . runParse input_line
    where
        input_line = State <$> statement <|> Calculation <$> expr


new_table :: SymbolTable -> InputLine -> Result SymbolTable
new_table table (State stmnt) = interpStatement table stmnt
new_table table _ = pure table

calculate :: SymbolTable -> InputLine -> Result String
calculate table (Calculation e) = show <$> interpExpr table e
calculate table (State (Set v r)) = ((v <> " = ") <>) <$> show <$> interpExpr table r

tuple_seq :: (Either a b, Either a c) -> Either a (b, c) 
tuple_seq ((Right a), (Right b)) = Right (a, b)
tuple_seq ((Left a), _) = Left a
tuple_seq (_, (Left b)) = Left b

_loop :: SymbolTable -> String -> Either Exception (SymbolTable, String)
_loop table line = do
    val <- parse_input line
    tuple_seq (new_table table val, calculate table val)
    
loop :: SymbolTable -> IO ()
loop table = do
    putStr "> "
    hFlush stdout
    x <- filter (not . flip elem " \t\r\n") <$> getLine
    let new_vals = _loop table x
    case new_vals of
      (Left e) -> print e >> loop table
      (Right (new_vartable, val)) -> putStrLn val >> loop new_vartable

main :: IO ()
main = loop Map.empty
