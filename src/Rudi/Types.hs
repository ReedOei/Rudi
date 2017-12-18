module Rudi.Types
    (
        Expr(..),
        RudiFile(..),
        Statement(..),
        Command(..)
    ) where

newtype RudiFile = RudiFile [Statement]
    deriving Show

data Command = ShowDef

data Statement = Import String |
                 Define Expr Expr |
                 EmptyStatement |
                 InternalCommand Command String

data Expr = Apply Expr Expr |
            Var String |
            ToSubstitute String |
            S | K | I | B | C
    deriving (Eq, Ord)

-- Numbers have to be written Succ (Succ (Succ Z))) for this to work.
getNumber (Var "Z") = Just 0
getNumber (Apply (Var "Succ") y) = (+ 1) <$> getNumber y
getNumber _ = Nothing

instance Show Expr where
    show (Var x) = x
    show S = "S"
    show K = "K"
    show I = "I"
    show B = "B"
    show C = "C"
    show expr =
        case getNumber expr of
            Just n -> show n
            Nothing ->
                case expr of
                    Apply x y@(Apply a b) -> show x ++ " (" ++ show y ++ ")"
                    Apply x y ->  show x ++ " " ++ show y

instance Show Statement where
    show (Import str) = "import " ++ str
    show (Define x y) = show x ++ " → " ++ show y
    show EmptyStatement = ""
