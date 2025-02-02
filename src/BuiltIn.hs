module BuiltIn where

import Prelude hiding ( const )
import Control.Monad
import Lang

instance Num Const where
    (CInt x) + (CInt y) = CInt (x + y)
    (CInt x) - (CInt y) = CInt (x - y)
    (CInt x) * (CInt y) = CInt (x * y)
    abs (CInt x) = CInt (abs x)
    signum (CInt x) = CInt (signum x)
    negate (CInt x) = CInt (negate x)

instance Ord Const where
  compare (CInt x) (CInt y) = compare x y

evalArithmetic :: Term -> Maybe Const 
evalArithmetic (TConst c) = Just c
evalArithmetic (CTerm "+" [x, y]) = (+) <$> evalArithmetic x <*> evalArithmetic y
evalArithmetic (CTerm "-" [x, y]) = (-) <$> evalArithmetic x <*> evalArithmetic y
evalArithmetic (CTerm "*" [x, y]) = (*) <$> evalArithmetic x <*> evalArithmetic y
    --(evalArithmetic x) + (evalArithmetic y)
evalArithmetic _ = Nothing


is_2 :: Term -> Term -> Maybe [Subst]
is_2 (TVar var) term = evalArithmetic term >>= (\a -> Just [(var, TConst a)])
is_2 term1 term2 = do 
    res <- evalArithmetic term2
    guard (term1 /= TConst res)
    return []
is_2 _ _ = Nothing

gt_2 :: Term -> Term -> Maybe Bool
gt_2 term1 term2 = do 
    e1 <- evalArithmetic term1
    e2 <- evalArithmetic term2
    return $ e1 > e2 

gte_2 :: Term -> Term -> Maybe Bool
gte_2 term1 term2 = do 
    e1 <- evalArithmetic term1
    e2 <- evalArithmetic term2
    return $ e1 >= e2 

lt_2 :: Term -> Term -> Maybe Bool
lt_2 term1 term2 = do 
    e1 <- evalArithmetic term1
    e2 <- evalArithmetic term2
    return $ e1 < e2 

boolToResult :: [Subst] -> Maybe Bool -> Maybe ResultsTree
boolToResult substs (Just True) = return $ RLeaf $ Just substs
boolToResult substs (Just False) = return $ RLeaf Nothing
boolToResult substs Nothing = Nothing