module Rudi.Types
    (
        Expr(..),
        RudiFile(..),
        Statement(..),
    ) where

newtype RudiFile = RudiFile [Statement]
    deriving Show

data Statement = Import String |
                 Define Expr Expr |
                 EmptyStatement

data Expr = Apply Expr Expr |
            Var String |
            ToSubstitute String |
            S | K | I | B | C
    deriving (Eq, Ord)


instance Show Expr where
    show (Var x) = x
    show S = "S"
    show K = "K"
    show I = "I"
    show B = "B"
    show C = "C"
    show (Apply x y@(Apply a b)) = show x ++ " (" ++ show y ++ ")"
    show (Apply x y) = show x ++ " " ++ show y

instance Show Statement where
    show (Import str) = "import " ++ str
    show (Define x y) = show x ++ " → " ++ show y
    show EmptyStatement = ""
