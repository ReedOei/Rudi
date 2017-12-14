module Rudi.Interpreter
    (
        eval
    ) where

import Data.Char (isUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import Rudi.Types

match :: Expr -> Expr -> Maybe (Map String Expr)
match (Var x) expr
    | isUpper $ head x =
        case expr of
            Var y | x == y -> Just $ Map.fromList [(x, Var y)]
            _ -> Nothing
    | otherwise = Just $ Map.fromList [(x, expr)]
match (Apply x y) (Apply a b) = Map.union <$> match x a <*> match y b
match (Apply _ _) (Var _) = Nothing

substituteName :: String -> Expr -> Expr -> Expr
substituteName _ _ (Var x) = Var x
substituteName name rep (ToSubstitute x)
    | x == name = rep
    | otherwise = ToSubstitute x
substituteName name rep (Apply x y) = Apply (substituteName name rep x) (substituteName name rep y)

-- Preparing to substitute creates placeholders for each variable we are going to substitute.
-- This is important if we are going to substitute value that have the same name as parameters in our rule.
-- Example: S K y x would substitute the value 'x' into the x position: (x x) (y x), then substitute K in for x, which would be wrong.
prepareSubstitute :: String -> Expr -> Expr
prepareSubstitute _ (ToSubstitute x) = ToSubstitute x
prepareSubstitute name (Var x)
    | x == name = ToSubstitute x
    | otherwise = Var x
prepareSubstitute name (Apply x y) = Apply (prepareSubstitute name x) (prepareSubstitute name y)

substitute :: Expr -> Expr -> Expr -> Expr
substitute search rep expr@(Var x) =
    case match search expr of
        Nothing -> expr
        Just reps -> Map.foldWithKey substituteName (Map.foldWithKey (\key _ cur -> prepareSubstitute key cur) rep reps) reps
substitute search rep expr@(Apply x y) =
    case match search expr of
        Nothing -> Apply (substitute search rep x) (substitute search rep y)
        Just reps -> Map.foldWithKey substituteName (Map.foldWithKey (\key _ cur -> prepareSubstitute key cur) rep reps) reps

doSubstitute :: Map Expr Expr -> Expr -> Expr
doSubstitute defs expr = Map.foldWithKey substitute expr defs

eval :: Map Expr Expr -> Expr -> Expr
eval defs (Var x) =
    case doSubstitute defs $ Var x of
        Var y | x == y -> Var y
        newExpr ->
            eval defs newExpr
-- eval defs (Apply (Apply (Var "K") x) y) = eval defs $ doSubstitute defs x
-- eval defs (Apply (Apply (Apply (Var "S") x) y) z) = eval defs $ Apply (Apply x z) (Apply y z)
eval defs expr@(Apply x0 y0) =
    case doSubstitute defs expr of
        Var x -> Var x
        Apply x y ->
            let evalX = eval defs $ doSubstitute defs x
                evalY = eval defs $ doSubstitute defs y in
                if evalX == x0 && evalY == y0 then
                    Apply evalX evalY
                else
                    eval defs $ Apply evalX evalY
