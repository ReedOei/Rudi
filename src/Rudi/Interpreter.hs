module Rudi.Interpreter
    (
        eval, evalOnce,
        compile, compileExpr
    ) where

import Data.Char (isUpper)
import Data.Map (Map)
import qualified Data.Map as Map

import System.IO.Unsafe

import Rudi.Types

contains :: Expr -> String -> Bool
contains (Var x) str = str == x
contains (Apply x y) str = x `contains` str || y `contains` str
contains _ _ = False

getFunctionName :: Expr -> String
getFunctionName (Var x) = x
getFunctionName (Apply x _) = getFunctionName x

prependParam :: String -> Expr -> Expr
prependParam str (Var x) = Apply (Var x) (Var str)
prependParam str (Apply x y) = Apply (prependParam str x) y

-- Takes a definition and redefines it using only S and K
compile :: Map Expr Expr -> Statement -> Statement
compile defs (Define def y) =
    let fname = getFunctionName def in
        if y `contains` fname then
            let newBody = doSubstitute (Map.fromList [(Var fname, Var "Func")]) y
                newDef = prependParam "Func" def in

                case compile defs (Define newDef newBody) of
                    Define x y -> Define (Var fname) $ doSubstitute defs $ Apply (Var "Y") y -- TODO: Maybe maybe this nice so I don't have to hardcode in the Y var.
        else
            case def of
                Var x -> Define (Var x) y
                Apply (Var x) (Var var) -> Define (Var x) $ compileExpr (doSubstitute defs y) var
                Apply x (Var var) -> compile defs $ Define x $ compileExpr (doSubstitute defs y) var

-- Bracket abstraction.
-- [x]_x = S K K
-- [x]_y = K x
-- [x y]_b = S ([x]_b) ([y]_b)
compileExpr :: Expr -> String -> Expr
compileExpr expr str
    | expr `contains` str =
        case expr of
            Var x | x == str -> Apply (Apply S K) K
            Apply x y -> Apply (Apply S (compileExpr x str)) (compileExpr y str)
    | otherwise = Apply K expr

-- Matches an expression with another expression.
-- Returns Nothing if they cannot be matched, otherwise, returns a Map containing
-- the substitutions to make (e.g., I -> S K K, or x -> (K x))
match :: Expr -> Expr -> Maybe (Map String Expr)
match (Var x) expr
    | isUpper $ head x =
        case expr of
            Var y | x == y -> Just $ Map.fromList [(x, Var y)]
            _ -> Nothing
    | otherwise = Just $ Map.fromList [(x, expr)]
match (Apply x y) (Apply a b) = Map.union <$> match x a <*> match y b
match (Apply _ _) (Var _) = Nothing
match _ _ = Nothing

substituteName :: String -> Expr -> Expr -> Expr
substituteName _ _ S = S
substituteName _ _ K = K
substituteName _ _ (Var x) = Var x
substituteName name rep (ToSubstitute x)
    | x == name = rep
    | otherwise = ToSubstitute x
substituteName name rep (Apply x y) = Apply (substituteName name rep x) (substituteName name rep y)

-- Preparing to substitute creates placeholders for each variable we are going to substitute.
-- This is important if we are going to substitute value that have the same name as parameters in our rule.
-- Example: S K y x would substitute the value 'x' into the x position: (x x) (y x), then substitute K in for x, which would be wrong.
prepareSubstitute :: String -> Expr -> Expr
prepareSubstitute _ S = S
prepareSubstitute _ K = K
prepareSubstitute _ (ToSubstitute x) = ToSubstitute x
prepareSubstitute name (Var x)
    | x == name = ToSubstitute x
    | otherwise = Var x
prepareSubstitute name (Apply x y) = Apply (prepareSubstitute name x) (prepareSubstitute name y)

substitute :: Expr -> Expr -> Expr -> Expr
substitute _ _ S = S
substitute _ _ K = K
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

-- Evaluates once, returning whether the expression was changed.
-- This is here because if we compare equality we force computation, which ruins the laziness.
evalOnce :: Expr -> (Bool, Expr)
evalOnce (Apply (Apply K x) y) = (True, x)
evalOnce (Apply (Apply (Apply S x) y) z) = (True, Apply (Apply x z) (Apply y z))
evalOnce (Apply x y) = let (xChange, evalX) = evalOnce x
                           (yChange, evalY) = evalOnce y in
                           (xChange || yChange, Apply evalX evalY)
evalOnce expr = (False, expr)

eval :: Map Expr Expr -> Expr -> Expr
eval defs expr =
    case expr of
        S -> S
        K -> K
        Var x ->
            case doSubstitute defs $ Var x of
                Var y | x == y -> Var y
                newExpr -> eval defs $ newExpr
        Apply (Apply K x) y -> eval defs $ x
        Apply (Apply (Apply S x) y) z -> eval defs $ Apply (Apply x z) (Apply y z)
        Apply x y ->
            let (xChange, evalX) = evalOnce $ doSubstitute defs x
                (yChange, evalY) = evalOnce $ doSubstitute defs y in
                if xChange || yChange then
                    eval defs $ Apply evalX evalY
                else
                    Apply evalX evalY

