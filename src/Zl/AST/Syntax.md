# Syntax Parsers

```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Zl.AST.Syntax where
```

## Expressions

An expression is either:

1. a literal value (int, double, boolean, string)
2. a constant (token)
3. a function call, consisting of a expr that returns a function, followed by n 
   expressions surrounded by parentheses and separated by commas
4. a binary operator application consisting of two exprs separated by an 
   operator in between them.
5. A prefix operator application consisting of a prefix operator token, followed 
   by an expr

We can translate this directly into Haskell:

```haskell
data Expr = Lit Primitive
          | Const Token
          | Call Expr [Expr]
          | BinOp Op Expr Expr
          | Prefix Op Expr
          deriving (Show, Eq, Ord)

```

And of course, let's not forget to expand the subtypes of expr:

```haskell
data Primitive = PInt Integer
               | PFloat Double
               | PBool Bool
               | PString String
               | PLambda [Token] Expr
               deriving (Show, Eq, Ord)

newtype Token = Token String deriving (Show, Eq, Ord)

data Op = Plus
        | Times
        | Minus
        | Divide
        | Negate
        | FloorDiv 
        | Modulo
        | Pow
        | And 
        | Or 
        | Not 
        | Append
        deriving (Show, Eq, Ord, Enum)
```
