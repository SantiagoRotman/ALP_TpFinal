{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Subst
Description : Define las operaciones de las substituciones
-}

module Subst where

import Lang

applySubstitutions :: Term -> [Subst] -> Term
applySubstitutions = foldl applySubstitution

applySubstitution :: Term -> Subst -> Term 
applySubstitution t2@(TVar y) (x, t1) = if x == y then t1 else t2
applySubstitution (CTerm thead body) subst  = CTerm thead (map (`applySubstitution` subst) body)
applySubstitution t _ = t

isByProductSubst :: Subst -> Bool
isByProductSubst ('_':_, t) = False
isByProductSubst _ = True

aggregateSubsts :: [Subst] -> [Subst]
aggregateSubsts [] = []
aggregateSubsts (x:xs) = foldl aggregateSubst x xs : aggregateSubsts xs 

aggregateSubst :: Subst -> Subst -> Subst
aggregateSubst s@(x, TVar xy) (y, yt) = if xy == y then (x, yt) else s 
aggregateSubst s1 s2 = s1