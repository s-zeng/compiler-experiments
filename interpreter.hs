{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Either
import Data.Map (Map)
import Data.List
import System.IO
import qualified Data.Map as Map

data Parser a = Parser { runParse :: String -> [(a, String)] } deriving (Functor)

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    f <*> g = Parser $ \s -> do (f, s1) <- runParse f s
                                (a, s2) <- runParse g s1
                                pure $ (f a, s2)

instance Alternative Parser where
    empty = Parser $ \s -> []
    f <|> g = Parser $ \s -> case runParse f s of
                               [] -> runParse g s
                               res -> res

instance Semigroup (Parser a) where
    f <> g = Parser $ \s -> runParse f s <> runParse g s
instance Monoid (Parser a) where
    mempty = empty

char :: Char -> Parser Char
char x = Parser $ \s -> case s of
                          (c:cs) | x == c -> [(c, cs)]
                          _ -> []

string :: String -> Parser String
string [] = pure []
string (x:xs) = (:) <$> char x <*> string xs

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty
oneOfChar = oneOf . fmap char
oneOfString = oneOf . fmap string

zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne x = Just <$> x <|> pure Nothing

is_not :: Parser a -> Parser ()
is_not f = Parser $ \s -> case runParse f s of
                         [] -> [((), s)]
                         _  -> []

parens :: Parser a -> Parser a
parens f = do
    char '('
    x <- f
    char ')'
    pure x

digit, leading_digit :: Parser Char
digit         = oneOfChar ['0'..'9']
leading_digit = oneOfChar ['1'..'9']

natural :: Parser Integer
natural = read <$> do
    lead <- leading_digit
    rest <- many digit
    pure $ lead:rest

zero :: Parser Integer
zero = char '0' *> pure 0

integer :: Parser Integer
integer = zero <|> do
    minus <- zeroOrOne $ char '-'
    num <- natural
    pure $ case minus of
             Nothing -> num
             _ -> negate num

floating :: Parser Double
floating = read <$> do 
    lead <- integer
    char '.'
    decimals <- many digit
    pure $ show lead <> "." <> decimals

-- ========================================================================== --
-- ========================================================================== --
-- ========================================================================== --
-- ========================================================================== --

data Op = Plus | Times | Minus | Div deriving (Show, Eq, Ord, Enum)
data PrimType = PInt | PFloat | PString | PBool deriving (Show, Eq, Ord, Enum)
data Primitive a = Primitive a PrimType deriving (Eq, Ord)
instance Show a => Show (Primitive a) where show (Primitive a t) = show a

data Expr = Number Double | Var String | BinOp Op Expr Expr deriving (Eq, Ord)

instance Show Expr where
    show (Number x)     = show x
    show (Var x)        = x
    show (BinOp op a b) = "(" <> show op <> " " <> show a <> " " <> show b <> ")"

charToOp :: Char -> Op
charToOp '+' = Plus
charToOp '-' = Minus
charToOp '*' = Times
charToOp '/' = Div

addOp, mulOp :: Parser Op
addOp = charToOp <$> oneOfChar "+-"
mulOp = charToOp <$> oneOfChar "*/"

chainOp :: Parser Expr -> Parser Op -> Parser Expr
chainOp arg op = do
    first_arg <- arg
    rest <- many $ do o <- op
                      a <- arg
                      pure $ (flip $ BinOp o) a 
    pure $ foldl' (&) first_arg rest

_token :: Parser String
_token = do
    x <- oneOfChar ['A'..'z'] <|> char '_'
    rest <- many $ oneOfChar ['A'..'z'] <|> oneOfChar ['0'..'9'] <|> char '_'
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
opToFunc Plus = liftA2 (+)
opToFunc Times = liftA2 (*)
opToFunc Minus = liftA2 (flip subtract)
opToFunc Div = \x y -> do
    a <- x
    b <- y
    if (b == 0) then Left DivideByZero
                else pure (a/b)

interpExpr :: SymbolTable -> Expr -> Result Double
interpExpr table (Number x) = Right x
interpExpr table (Var s) = case Map.lookup s table of
                             Nothing -> Left (Undefined s)
                             (Just x) -> Right x
interpExpr table (BinOp op x y) = opToFunc op (interpExpr table x) (interpExpr table y)

-- ========================================================================== --

interpStatement :: SymbolTable -> Statement -> Result SymbolTable
interpStatement table (Set x expr) = do
    val <- interpExpr table expr
    pure $ Map.insert x val table

-- ========================================================================== --

data InputLine = Calculation Expr | State Statement deriving (Eq, Ord)
instance Show InputLine where
    show (Calculation x) = show x
    show (State (Set v r)) = v <> " = "
input_line = State <$> statement <|> Calculation <$> expr
parse_input = process_parse . runParse input_line

new_table :: SymbolTable -> InputLine -> Result SymbolTable
new_table table (State stmnt) = interpStatement table stmnt
new_table table _ = pure table

calculate :: SymbolTable -> InputLine -> Result String
calculate table (Calculation expr) = show <$> interpExpr table expr
calculate table (State (Set v r)) = ((v <> " = ") <>) <$> show <$> interpExpr table r

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
      (Right (new_table, val)) -> putStrLn val >> loop new_table

main :: IO ()
main = loop Map.empty
