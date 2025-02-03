{-|
Module      : Global
Description : Define el estado global del interprete
-}
module Global where

import Lang

data GlEnv = GlEnv {
  traceDebug :: Bool,
  inter :: Bool,        --  ^ True, si estamos en modo interactivo.
                        -- Este parámetro puede cambiar durante la ejecución:
                        -- Es falso mientras se cargan archivos, pero luego puede ser verdadero.
  lfile :: String,      -- ^ Último archivo cargado.
  cantClause :: Int,      -- ^ Cantidad de declaraciones desde la última carga
  glb :: [Expr]  -- ^ Entorno con declaraciones globales
}

data Mode =
    Interactive
  | Eval
  | Debug
  deriving (Show, Eq)

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv False False "" 0 []

data Conf = Conf {
    opt :: Bool,
    modo :: Mode
}