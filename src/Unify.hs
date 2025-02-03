module Unify (runUnifier, solveQuery, areEquals) where

import Prelude hiding ( const )
import Lang
import Control.Monad.State
--import Control.Monad.Reader
import Data.Maybe

import Global
import BuiltIn
import Debug.Trace

import PPrint hiding (applySubstitutions, applySubstitution)

-----------------------------------------------------
--                 Monada UnifyM                   --
-----------------------------------------------------

type UnifyM a = State Int a
--type UnifyM a = ReaderT GlEnv (StateT Int IO) a

runUnifyM :: State a b -> a -> (b, a)
runUnifyM = runState

runUnifyM_ :: State a b -> a -> b
runUnifyM_ m a = let (x, _) = runUnifyM m a in x

uFail :: UnifyM Result
uFail = return Nothing

getTrace :: GlEnv -> Bool
getTrace = traceDebug

getClauses' :: GlEnv -> Int -> [Expr]
getClauses' env n = let
    expr = glb env
    in map (replaceVarExpr (n+1)) expr

replaceVarExpr :: Int -> Expr -> Expr
replaceVarExpr i (Fact t) = Fact (replaceVarTerm i t)
replaceVarExpr i (Rule t tree) = Rule (replaceVarTerm i t) (replaceVarTree i tree)
replaceVarExpr i a = a

replaceVarTerm :: Int -> Term -> Term
replaceVarTerm i (TVar var) = TVar ("_"++ var ++ "_" ++ show i)
replaceVarTerm i (CTerm a xs) = CTerm a (map (replaceVarTerm i) xs)
replaceVarTerm i a = a

replaceVarTree :: Int -> TermOpTree -> TermOpTree
replaceVarTree i (Node op l r) = Node op (replaceVarTree i l) (replaceVarTree i r)
replaceVarTree i (Leaf a) = Leaf (replaceVarTerm i a)

freshVar :: UnifyM Term
freshVar = do
    n <- get
    put (n+1)
    return $ TVar ("_" ++ show n)

-----------------------------------------------------
--                 Substitutions                   --
-----------------------------------------------------

mergeSubstitution  :: Subst -> [Subst] -> UnifyM Result
mergeSubstitution x [] = return $ Just [x]
mergeSubstitution (v1, t1) ((v2, t2):ys) 
    | v1 == v2 = do  -- Si las variables son iguales trato de unificar los terminos
        mxs <- unifyTerms t1 t2
        mys <- mergeSubstitution (v1, t1) ys
        return $ liftM2 (++) mxs mys
    | otherwise = mergeSubstitution (v1, t1) ys

mergeSubstitutions :: [Subst] -> [Subst] -> UnifyM Result
mergeSubstitutions [] ys = return $ Just ys
mergeSubstitutions xs [] = return $ Just xs
mergeSubstitutions (x:xs) ys = do
    --trace ("ms: " ++ (show (x:xs)) ++ " - " ++ (show ys)) (return [])
    newSubst <- mergeSubstitution x ys
    res <- mergeSubstitutions xs ys
    let aux = liftM2 (++) newSubst res
    --trace (show aux) (return [])
    return aux

mergeSubstitutions' :: [Subst] -> Maybe [Subst] -> UnifyM Result
mergeSubstitutions' xs = maybe (return Nothing) (mergeSubstitutions xs) 
mergeSubstitutions'' :: Maybe [Subst] -> Maybe [Subst] -> UnifyM Result
mergeSubstitutions'' mx my = maybe (return Nothing) (\a -> maybe (return Nothing) (mergeSubstitutions a) my) mx

applySubstitutions :: Term -> [Subst] -> Term
applySubstitutions = foldl applySubstitution

applySubstitution :: Term -> Subst -> Term 
applySubstitution t2@(TVar y) (x, t1) = if x == y then t1 else t2
applySubstitution (CTerm thead body) subst  = CTerm thead (map (`applySubstitution` subst) body)
applySubstitution t _ = t

-----------------------------------------------------
--                     Unify                       --
-----------------------------------------------------

unifyTerms :: Term -> Term -> UnifyM Result
unifyTerms (TAtom a1) (TAtom a2) 
    | a1 == a2  = return $ Just []
    | otherwise = uFail
unifyTerms (TConst c1) (TConst c2)
    | c1 == c2  = return $ Just []
    | otherwise = uFail
unifyTerms v1@(TVar x) v2@(TVar y)
    | x == y = return $ Just []
    | otherwise = do 
        z <- freshVar
        return $ Just [(x, z), (y, z)] -- Instanciamos las dos variables distintas a una nueva
-- Quizas hay que verificar que x no este en t 
unifyTerms var@(TVar x) t = return $ Just [(x, t)] -- Instanciar x, Variable Elimination
unifyTerms t var@(TVar x) = return $ Just [(x, t)] -- Instanciar x, Variable Elimination
unifyTerms (CTerm head1 body1) (CTerm head2 body2)  -- Term Reduction
    | head1 == head2 && length body1 == length body2 = unifyArgs body1 body2
    | otherwise = uFail
unifyTerms _ _ = uFail

unifyArgs :: [Term] -> [Term] -> UnifyM Result
unifyArgs [] [] = return $ Just []
unifyArgs (x:xs) (y:ys) = do 
    xs' <- unifyTerms x y
    ys' <- unifyArgs xs ys
    i <- get
    mergeSubstitutions'' ys' xs'
unifyArgs _ _ = uFail

----------------------------------------------------------------------------------------------------------------------
------------------------------------------------------SOLVE-----------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------
--                    solveExpr                    --
-----------------------------------------------------

solveExpr :: GlEnv -> Int -> [Subst] -> Term -> Expr -> ResultsTree
solveExpr glbEnv i substs t1 (Fact t2) = let
    (s, i') = runUnifyM (unifyTerms (applySubstitutions t1 substs) t2) i
    (s',_) = runUnifyM (mergeSubstitutions' substs s) i'
    in RLeaf s'
solveExpr glbEnv i substs t (Rule thead body) = let
    (substHead, i') = runUnifyM (unifyTerms (applySubstitutions t substs) thead) i
    (substs', i'') = runUnifyM (mergeSubstitutions' substs substHead) i'
    tree = maybe (RLeaf Nothing) (\a -> solveQueryTree glbEnv i'' a body) substs' --solveQueryTree' glbEnv i'' substs' body
    in tree --treeApply (\a -> runUnifyM_ (mergeSubstitutions' substs a) i') tree
solveExpr glbEnv i substs t (Query _) = RLeaf Nothing -- Queries no deberian ser parte de la KB

-----------------------------------------------------
--                    SolveTerm                    --
-----------------------------------------------------

solveTermBuiltIn :: GlEnv -> Int -> [Subst] -> Term -> Maybe ResultsTree
solveTermBuiltIn glbEnv i substs (CTerm "=" [t1,t2]) = do  
    let (result, i') = runUnifyM (unifyTerms (applySubstitutions t1 substs) (applySubstitutions t2 substs)) i
    result' <- runUnifyM_ (mergeSubstitutions' substs result) i'
    return $ RLeaf $ Just result'
solveTermBuiltIn glbEnv i substs (CTerm "\\=" [t1,t2]) = do
    let result = runUnifyM_ (unifyTerms (applySubstitutions t1 substs) (applySubstitutions t2 substs)) 0
    case result of
        Just xs -> return $ RLeaf Nothing
        Nothing -> return $ RLeaf $ Just substs
solveTermBuiltIn glbEnv i substs (CTerm "\\+" [t]) = do
    let result = solveTerm glbEnv i substs (applySubstitutions t substs)
    if hasResult result 
        then return $ RLeaf Nothing
        else return $ RLeaf $ Just substs
solveTermBuiltIn glbEnv i substs (CTerm "is" [t1,t2]) = do
    let result = is_2 (applySubstitutions t1 substs) (applySubstitutions t2 substs)
    --_ <- trace (show result) (Just $ RLeaf Nothing)
    result' <- runUnifyM_ (mergeSubstitutions' substs result) i
    return $ RLeaf $ Just result'
solveTermBuiltIn glbEnv i substs (CTerm ">" [t1,t2])  = generalBinaryFun gt_2 (boolToResult substs) substs t1 t2 
solveTermBuiltIn glbEnv i substs (CTerm "<" [t1,t2])  = generalBinaryFun lt_2 (boolToResult substs) substs t1 t2  
solveTermBuiltIn glbEnv i substs (CTerm ">=" [t1,t2]) = generalBinaryFun gte_2 (boolToResult substs) substs t1 t2  
solveTermBuiltIn glbEnv i substs (CTerm "print" body) = do
    let substs' = aggregateSubsts substs
    let xs = ppTerms $ map (`applySubstitutions` substs') body
    let xs' = map (\t -> ppTerm (applySubstitutions t substs')) body
    mapM_ (\s -> trace s (Just $ RLeaf Nothing)) xs'
    return $ RLeaf $ Just (substs ++ [("_PRINT_", TConst (CString ( xs)))])
solveTermBuiltIn glbEnv i substs _ = Nothing

generalBinaryFun :: (Term -> Term -> Maybe a) -> (Maybe a -> Maybe ResultsTree) -> [Subst] -> Term -> Term -> Maybe ResultsTree
generalBinaryFun fun converter substs t1 t2 = converter $ fun (applySubstitutions t1 substs) (applySubstitutions t2 substs)

solveTermKB :: GlEnv -> Int -> [Subst] -> Term -> ResultsTree
solveTermKB glbEnv i substs t = let
    clauses = getClauses' glbEnv i
    results = filter isNotEmptyLeaf $ map (solveExpr glbEnv (i+1) substs t) clauses
    in if null results then RLeaf Nothing else RNode results

solveTerm :: GlEnv -> Int -> [Subst] -> Term -> ResultsTree
solveTerm glbEnv i s t | if getTrace glbEnv then
                             trace ("solveTerm: " ++ show i ++ ", " ++ ppTerm (applySubstitutions t s) 
                             ++ ", result: " ++ ppResultsTreeInLine (fromMaybe (solveTermKB glbEnv i s t) (solveTermBuiltIn glbEnv i s t))) False 
                             else False = undefined
solveTerm glbEnv i substs t = fromMaybe (solveTermKB glbEnv i substs t) (solveTermBuiltIn glbEnv i substs t)

-----------------------------------------------------
--                    SolveQuery                   --
-----------------------------------------------------

solveQueryTree :: GlEnv -> Int -> [Subst] -> TermOpTree -> ResultsTree
solveQueryTree glbEnv i substs (Node And l r) = let
    lSubsts = solveQueryTree glbEnv i substs l
    in treeMap (maybe (RLeaf Nothing) (\a -> solveQueryTree glbEnv i a r)) lSubsts
    --in treeMap (\a -> solveQueryTree' glbEnv i a r)  
solveQueryTree glbEnv i substs (Node Or l r) = let 
    lSubsts = solveQueryTree glbEnv i substs l
    rSubsts = solveQueryTree glbEnv i substs r
    in RNode [lSubsts, rSubsts]
solveQueryTree glbEnv i substs (Leaf a) = solveTerm glbEnv i substs a

solveQuery :: GlEnv -> Int -> Expr -> ResultsTree
solveQuery glbEnv i q | if getTrace glbEnv then trace ("solveQuery: " ++ show i ++ " " ++ show q) False else False = undefined
solveQuery glbEnv i (Query tree) = solveQueryTree glbEnv i [] tree
solveQuery glbEnv i e = RLeaf Nothing

runUnifier :: (GlEnv -> Int -> a -> ResultsTree) -> GlEnv -> a -> ResultsTree
runUnifier unifier glbEnv x = unifier glbEnv 0 x 
    --let (result, _) = runUnifyM (runReaderT (unifier x) glbEnv) 0 in result

areEquals :: Term -> Term -> Maybe [Subst]
areEquals t1 t2 = case runUnifyM (unifyTerms t1 t2) 0 of
        (Just subst, _) -> Just subst
        (Nothing, _) -> Nothing
