module Rudi.Types
    (
        Expr(..),
        RudiFile(..),
        Statement(..)
    ) where

newtype RudiFile = RudiFile [Statement]
    deriving Show

data Statement = Import String |
                 Define Expr Expr |
                 EmptyStatement
    deriving Show

data Expr = Apply Expr Expr |
            Var String |
            ToSubstitute String
    deriving (Eq, Ord)

instance Show Expr where
    show (Var x) = x
    show (Apply x y) = "(" ++ show x ++ " " ++ show y ++ ")"
