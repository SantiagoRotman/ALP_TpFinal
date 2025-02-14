module Unify (runUnifier, solveQuery, areEquals) where

import Prelude hiding ( const )
import Lang
import Control.Monad.State
--import Control.Monad.Reader
import Data.Maybe

import Global
import Subst
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

-- Remplaza los nombres de las variables de la KB por unos unicos
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
    | v1 == v2 && t1 == t2 = return $ Just [] -- Si esta repetida no es necesaria agregarla
    | v1 == v2 = unifyTerms t1 t2 -- Si las variables son iguales trato de unificar los terminos
        {-do 
        mxs <- unifyTerms t1 t2
        mys <- mergeSubstitution (v1, t1) ys
        return mxs -- $ liftM2 (++) mxs mys -}
    | otherwise = mergeSubstitution (v1, t1) ys

mergeSubstitutions :: [Subst] -> [Subst] -> UnifyM Result
mergeSubstitutions [] ys = return $ Just ys
mergeSubstitutions xs [] = return $ Just xs
mergeSubstitutions (x:xs) ys = do
    newSubst <- mergeSubstitution x ys
    res <- mergeSubstitutions xs ys
    return $ liftM2 (++) newSubst res

mergeSubstitutions' :: [Subst] -> Maybe [Subst] -> UnifyM Result
mergeSubstitutions' xs = maybe (return Nothing) (mergeSubstitutions xs) 
mergeSubstitutions'' :: Maybe [Subst] -> Maybe [Subst] -> UnifyM Result
mergeSubstitutions'' mx my = maybe (return Nothing) (\a -> maybe (return Nothing) (mergeSubstitutions a) my) mx

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

-- Soluciona un termino contra una expresion
solveExpr :: GlEnv -> Int -> [Subst] -> Term -> Expr -> ResultsTree
solveExpr glbEnv i substs t1 (Fact t2) = let
    (s, i') = runUnifyM (unifyTerms (applySubstitutions t1 substs) t2) i -- Unifico los terminos
    (s',i'') = runUnifyM (mergeSubstitutions' substs s) i'
    in RLeaf i'' s'
solveExpr glbEnv i substs t (Rule thead body) = let
    (substHead, i') = runUnifyM (unifyTerms (applySubstitutions t substs) thead) i -- Unifico el termino con la cabecera
    (substs', i'') = runUnifyM (mergeSubstitutions' substs substHead) i'
    in maybe (RLeaf i'' Nothing) (\a -> solveQueryTree glbEnv i'' a body) substs' -- Soluciono el objetivo 
solveExpr glbEnv i substs t (Query _) = RLeaf i Nothing -- Queries no deberian ser parte de la KB

-----------------------------------------------------
--                    SolveTerm                    --
-----------------------------------------------------

-- Si el termino es uno de los BuiltIn, lo soluciona con su comportamiento especial, si no falla
solveTermBuiltIn :: GlEnv -> Int -> [Subst] -> Term -> Maybe ResultsTree
solveTermBuiltIn glbEnv i substs (CTerm ">" [t1,t2])  = generalBinaryFun gt_2 (boolToResult i substs) substs t1 t2 
solveTermBuiltIn glbEnv i substs (CTerm "<" [t1,t2])  = generalBinaryFun lt_2 (boolToResult i substs) substs t1 t2  
solveTermBuiltIn glbEnv i substs (CTerm ">=" [t1,t2]) = generalBinaryFun gte_2 (boolToResult i substs) substs t1 t2  
solveTermBuiltIn glbEnv i substs (CTerm "=:=" [t1,t2]) = generalBinaryFun eq_2 (boolToResult i substs) substs t1 t2 
solveTermBuiltIn glbEnv i substs (CTerm "is" [t1,t2]) = do
    let result = is_2 (applySubstitutions t1 substs) (applySubstitutions t2 substs)
    let (result', i') = runUnifyM (mergeSubstitutions' substs result) i
    return $ RLeaf i' result' 
solveTermBuiltIn glbEnv i substs (CTerm "=" [t1,t2]) = do  
    let (result, i') = runUnifyM (unifyTerms (applySubstitutions t1 substs) (applySubstitutions t2 substs)) i
    result' <- runUnifyM_ (mergeSubstitutions' substs result) i'
    return $ RLeaf i' $ Just result'
solveTermBuiltIn glbEnv i substs (CTerm "\\=" [t1,t2]) = do
    let (result, i') = runUnifyM (unifyTerms (applySubstitutions t1 substs) (applySubstitutions t2 substs)) 0
    case result of
        Just xs -> return $ RLeaf i' Nothing
        Nothing -> return $ RLeaf i' $ Just substs
solveTermBuiltIn glbEnv i substs (CTerm "\\+" [t]) = do
    let result = solveTerm glbEnv i substs (applySubstitutions t substs)
    if hasResult result 
        then return $ RLeaf i Nothing
        else return $ RLeaf i $ Just substs
solveTermBuiltIn glbEnv i substs (CTerm "print" body) = do
    let substs' = aggregateSubsts substs
    let xs = ppTerms $ map (`applySubstitutions` substs') body
    --let xs' = map (\t -> ppTerm (applySubstitutions t substs')) body
    --_ <- trace (show (filter (not . isPrint) substs)) (Just $ RLeaf Nothing)
    --mapM_ (\s -> trace s (Just $ RLeaf i Nothing)) xs'
    return $ RLeaf i $ Just (substs ++ [("_PRINT_", TConst (CString xs))])
solveTermBuiltIn glbEnv i substs _ = Nothing

generalBinaryFun :: (Term -> Term -> Maybe a) -> (Maybe a -> Maybe ResultsTree) -> [Subst] -> Term -> Term -> Maybe ResultsTree
generalBinaryFun fun converter substs t1 t2 = converter $ fun (applySubstitutions t1 substs) (applySubstitutions t2 substs)

-- Trata de solucionar un termino contra toda la KB
solveTermKB :: GlEnv -> Int -> [Subst] -> Term -> ResultsTree
solveTermKB glbEnv i substs t = let
    clauses = getClauses' glbEnv i
    results = filter isNotEmptyLeaf $ map (solveExpr glbEnv (i+1) substs t) clauses
    in if null results then RLeaf i Nothing else RNode results

-- Si es uno de los terminos BuiltIn lo soluciona con su comportamiento especial
--    si no lo trata de solucionar contra la KB
solveTerm :: GlEnv -> Int -> [Subst] -> Term -> ResultsTree
solveTerm glbEnv i s t | if getTrace glbEnv then
                             trace ("solveTerm: " ++ show i ++ ", " ++ ppTerm (applySubstitutions t s) 
                             ++ ", result: " ++ ppResultsTreeInLine (fromMaybe (solveTermKB glbEnv i s t) (solveTermBuiltIn glbEnv i s t))) False 
                             else False = undefined
solveTerm glbEnv i substs t = fromMaybe (solveTermKB glbEnv i substs t) (solveTermBuiltIn glbEnv i substs t)

-----------------------------------------------------
--                    SolveQuery                   --
-----------------------------------------------------

-- Soluciona un objetivo 
solveQueryTree :: GlEnv -> Int -> [Subst] -> TermOpTree -> ResultsTree
solveQueryTree glbEnv i substs (Node And l r) = let
    lSubsts = solveQueryTree glbEnv i substs l
    -- Trato de solucionar el lado derecho con los resultados no vacios generados por el arbol izquierdo
    in treeMap (\x -> maybe (RLeaf x Nothing) (\a -> solveQueryTree glbEnv x a r)) lSubsts
solveQueryTree glbEnv i substs (Node Or l r) = let 
    lSubsts = solveQueryTree glbEnv i substs l
    rSubsts = solveQueryTree glbEnv i substs r
    -- Ambos lados son soluciones posible
    in RNode [lSubsts, rSubsts]
solveQueryTree glbEnv i substs (Leaf a) = solveTerm glbEnv i substs a

solveQuery :: GlEnv -> Int -> Expr -> ResultsTree
solveQuery glbEnv i q | if getTrace glbEnv then trace ("solveQuery: " ++ show i ++ " " ++ show q) False else False = undefined
solveQuery glbEnv i (Query tree) = solveQueryTree glbEnv i [] tree
solveQuery glbEnv i e = RLeaf i Nothing

runUnifier :: (GlEnv -> Int -> a -> ResultsTree) -> GlEnv -> a -> ResultsTree
runUnifier unifier glbEnv x = unifier glbEnv 0 x 
    --let (result, _) = runUnifyM (runReaderT (unifier x) glbEnv) 0 in result

areEquals :: Term -> Term -> Maybe [Subst]
areEquals t1 t2 = case runUnifyM (unifyTerms t1 t2) 0 of
        (Just subst, _) -> Just subst
        (Nothing, _) -> Nothing
