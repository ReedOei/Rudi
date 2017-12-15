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
            S |
            K
    deriving (Eq, Ord)

instance Show Expr where
    show (Var x) = x
    show S = "S"
    show K = "K"
    show (Apply (Apply a b) y) = "(" ++ show a ++ " " ++ show b ++ " " ++ show y ++ ")"
    show (Apply x y) = "(" ++ show x ++ " " ++ show y ++ ")"

-- prettyPrint (Var x) = x
-- prettyPrint S = "S"
-- prettyPrint K = "K"
-- prettyPrint (Apply x y) = "(" ++ prettyPrint x ++ " " ++ prettyPrint y ++ ")"

instance Show Statement where
    show (Import str) = "import " ++ str
    show (Define x y) = show x ++ " → " ++ show y
    show EmptyStatement = ""
