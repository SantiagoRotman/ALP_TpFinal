{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

module PPrint where

import Lang
import Subst

import Control.Monad.State
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
    ( (<+>),
      annotate,
      defaultLayoutOptions,
      layoutSmart,
      vsep,
      hcat,
      hsep,
      space,
      punctuate, comma, line,
      Doc,
      Pretty(pretty) )

-- | Circular list state
type CList a = ([a], Int)
-- | Circular list monad
type CListMonad a = State (CList a)

getCol :: CListMonad a a
getCol = do 
  (list, idx) <- get
  return $ list !! idx

nextCol :: CListMonad a ()
nextCol = do 
  (list, idx) <- get
  let n = length list 
  put (list, (idx + 1) `mod` n)
  return ()

prevCol :: CListMonad a ()
prevCol = do 
  (list, idx) <- get
  let n = length list 
  put (list, (idx - 1) `mod` n)
  return ()

--Colores
type ColorTy = Doc AnsiStyle -> Doc AnsiStyle
type ParensColMonad = CListMonad ColorTy

-- | Encapsular documento con parentesis coloreados
colParens :: ColorTy -> Doc AnsiStyle -> Doc AnsiStyle
colParens col doc = col (pretty "(") <> doc <> col (pretty ")")  

-- | Lista de colores para parentesis
parenColList :: [Doc AnsiStyle -> Doc AnsiStyle]
parenColList = map annotate [colorDull Yellow, colorDull Magenta, colorDull Blue]

constColor :: ColorTy
constColor = annotate (color Red)
opColor :: ColorTy
opColor = annotate (colorDull Green <> bold) 
keywordColor :: ColorTy
keywordColor = annotate (colorDull Green) -- <> bold)
varColor :: ColorTy
varColor = annotate (color Blue <> italicized)
typeOpColor :: ColorTy
typeOpColor = annotate (colorDull Blue)
defColor :: ColorTy
defColor = annotate (colorDull Magenta)
defaultColor :: ColorTy
defaultColor = id

-- | Pretty printer de nombres (Doc)
atom2doc :: ColorTy -> Atom -> Doc AnsiStyle
atom2doc cl n = cl (pretty n)

-- | Pretty printer de nombres (Doc)
var2doc :: Variable -> Doc AnsiStyle
var2doc n = varColor (pretty n)

-- | Pretty printer de nombres (Doc)
const2doc :: Const -> Doc AnsiStyle
const2doc n = constColor (pretty $ show n)

-- | Pretty printer de operadores (Doc)
op2doc :: BoolOp -> Doc AnsiStyle
op2doc And = opColor (pretty ",")
op2doc Or  = opColor (pretty ";")

-- | Pretty printer de terminos (Doc)
term2doc :: Term -> ParensColMonad (Doc AnsiStyle)
term2doc (TConst c)         = return $ const2doc c
term2doc (TAtom at)         = return $ atom2doc defaultColor at
term2doc (TVar var)         = return $ var2doc var
term2doc (CTerm thead body) = do
  let thead' = atom2doc defaultColor thead
  parCol <- getCol
  nextCol
  body' <- terms2doc body
  prevCol
  return $ thead' <> colParens parCol body'

termWithColor2doc :: ColorTy -> Term -> ParensColMonad (Doc AnsiStyle)
termWithColor2doc col (TConst c)         = return $ const2doc c
termWithColor2doc col (TAtom at)         = return $ atom2doc col at
termWithColor2doc col (TVar var)         = return $ var2doc var
termWithColor2doc col (CTerm thead body) = do
  let thead' = atom2doc col thead
  parCol <- getCol
  nextCol
  body' <- terms2doc body
  prevCol
  return $ thead' <> colParens parCol body'
  
terms2doc :: [Term] -> ParensColMonad (Doc AnsiStyle)
terms2doc ts = do
    ts' <- mapM term2doc ts
    return $ hcat $ punctuate (pretty ", ") ts'
    --return $ sep ts'

-- | Pretty printer de terminos (Doc)
opTree2doc :: Bool -> TermOpTree -> ParensColMonad (Doc AnsiStyle)
opTree2doc _  (Leaf t)      = term2doc t
opTree2doc at (Node op l r) = do
    l' <- opTree2doc True l
    r' <- opTree2doc True r
    parenIf at $ hcat [l', op2doc op, space, r']

expr2doc :: Expr -> ParensColMonad (Doc AnsiStyle)
expr2doc (Fact t)      = termWithColor2doc defColor t
expr2doc (Query tree)  = do 
  body <- opTree2doc False tree
  return $ opColor (pretty "?-") <+> body
expr2doc (Rule t tree) = do
    thead <- termWithColor2doc defColor t
    body <- opTree2doc False tree
    return $ hsep [thead,opColor (pretty ":-"), body]

parenIf :: Bool -> Doc AnsiStyle -> ParensColMonad (Doc AnsiStyle)
parenIf True d = do
    parCol <- getCol
    return $ colParens parCol d
parenIf _ d = return d

ppTerm :: Term -> String
ppTerm e = let (s, _) = runState (term2doc e) (parenColList, 0) in render s

ppTerms :: [Term] -> String
ppTerms xs = let (s, _) = runState (terms2doc xs) (parenColList, 0) in render s

ppExpr :: Expr -> String
ppExpr e = let (s, _) = runState (expr2doc e) (parenColList, 0) in render s

----- Results -----

handleSubsts :: [Subst] -> [Subst]
handleSubsts substs = let substs' = aggregateSubsts substs in filter isByProductSubst $ map (applyToTerm (`applySubstitutions` substs')) substs' 

getPrints' :: [Subst] -> [Doc AnsiStyle]
getPrints' [] = []
getPrints' (("_PRINT_", TConst (CString s)):xs) = pretty s : getPrints' xs
getPrints' (x:xs) = getPrints' xs 

getPrints :: [Subst] -> Doc AnsiStyle
getPrints xs = let xs' = getPrints' xs in if null xs' then mempty else vsep xs' <> line

subst2doc :: Subst -> Doc AnsiStyle
subst2doc (var, t) = let (t',_) = runState (term2doc t) (parenColList, 0) in 
    hsep [var2doc var, pretty "=", t'] 

substs2doc :: [Subst] -> Doc AnsiStyle
substs2doc xs = let xs' = map subst2doc (handleSubsts xs) in if null xs' then pretty "True" else getPrints xs <> hsep (punctuate comma xs')

ppSubsts :: [Subst] -> String
ppSubsts = render . substs2doc

result2doc :: Result -> Doc AnsiStyle
result2doc Nothing   = pretty "False" <> line
result2doc (Just []) = pretty "True" <> line
result2doc (Just xs) = substs2doc xs <> line

ppResultsTree :: ResultsTree -> String
ppResultsTree (RLeaf _ result) = render $ result2doc result
ppResultsTree (RNode children) = concatMap ppResultsTree children

result2doc_ :: Result -> Doc AnsiStyle
result2doc_ Nothing   = pretty "False" 
result2doc_ (Just []) = pretty "True" 
result2doc_ (Just xs) = substs2doc xs 

resultsTree2doc :: ResultsTree -> Doc AnsiStyle
resultsTree2doc (RLeaf _ result) = result2doc_ result
resultsTree2doc (RNode children) = hsep $ punctuate comma (map resultsTree2doc children)

ppResultsTreeInLine :: ResultsTree -> String
ppResultsTreeInLine = render . resultsTree2doc

ppResult :: Result -> String
ppResult = render . result2doc

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

