{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}

{-|
Module      : Lang
Description : AST de términos, expresiones y tipos
-}

module Lang where

{-
  CLAUSES
    FACTS -> PREDICATE(ARGUMENTS)
      loves(vincent, mia).
      happy(yolanda).   
      listens2Music(mia).

    RULES -> HEAD :- BODY
      listens2Music(yolanda):- happy(yolanda).
      playsAirGuitar(mia):- listens2Music(mia).
      playsAirGuitar(yolanda):- listens2Music(yolanda).
      playsAirGuitar(vincent):- listens2Music(vincent), happy(vincent).     AND 
      playsAirGuitar(butch):- happy(butch); listens2Music(butch).           OR
      jealous(X,Y):- loves(X,Z), loves(Y,Z).

  QUERIES
    ?- happy(yolanda).
    ?- playsAirGuitar(yolanda).

    QUERIES con VARIABLES (empiezan con mayuscula)
    ?- woman(X).
    ?- loves(marsellus,X), woman(X).      AND

There are four kinds of term in Prolog: atoms, numbers, variables, and complex terms (or structures).
 Atoms and numbers are lumped together under the heading constants, and constants and variables together
 make up the simple terms of Prolog. 
-}

type Atom  = String
type Variable = String
--type Variable = Bound !Int

data Const = CInt Int
           | CString String
  deriving (Show, Eq)

data Term =
    TAtom Atom        -- |
  | TConst Const      -- | Heading Constants -- | 
  | TVar Variable     ------------------------- | Simple Terms
  | CTerm Atom [Term] --- Complex Terms
  deriving (Show, Eq)

applyToTerm :: (Term -> Term) -> Subst -> Subst
applyToTerm f (v, t) = (v, f t)

data BoolOp = Or | And
  deriving Show

data Tree op a = Node op (Tree op a) (Tree op a)
               | Leaf a
               deriving (Eq, Ord, Show)

type TermOpTree = Tree BoolOp Term -- Objetivo

data Expr =
    Fact Term
  | Rule Term TermOpTree
  | Query TermOpTree
  deriving (Show)

isClause :: Expr -> Bool
isClause (Query _) = False
isClause _ = True

type Module = [Expr]

-- Substitucion
type Subst = (Variable, Term)

data GeneralTree a = RNode [GeneralTree a]
                   | RLeaf Int a
                   deriving (Eq, Show) 


-- Resultados
type Result = Maybe [Subst] -- Resultado de unificar
type ResultsTree = GeneralTree Result -- Resultado de probar un objetivo

-- Funciones auxiliares
treeMap :: (Int -> Result -> ResultsTree) -> ResultsTree -> ResultsTree
treeMap f (RLeaf i s) = f i s
treeMap f (RNode xs) = RNode $ map (treeMap f) xs

treeApply :: (Result -> Result) -> ResultsTree -> ResultsTree
treeApply f (RLeaf i s)  = RLeaf i $ f s
treeApply f (RNode xs) = RNode $ map (treeApply f) xs

treeSize :: ResultsTree -> Int
treeSize (RLeaf i s) = 1
treeSize (RNode xs) = foldl (+) 1 $ map treeSize xs

isNotEmptyLeaf :: ResultsTree -> Bool
isNotEmptyLeaf (RLeaf i Nothing) = False
isNotEmptyLeaf (RLeaf _  _) = True
isNotEmptyLeaf _ = True

hasResult :: ResultsTree -> Bool
hasResult (RLeaf i (Just xs)) = True
hasResult (RNode xs) = any hasResult xs
hasResult _ = False
