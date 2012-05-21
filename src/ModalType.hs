{-# LANGUAGE
 TypeOperators
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      : ModalType
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable

module ModalType where

import Control.Monad.Writer.Strict (Writer, tell, execWriter)
import Control.Monad.Error
import Data.Monoid
import Data.Functor
import Data.Map as M
import Data.Set as S

type TpVar = String
type TmVar = String

data Tp = Tp :-> Tp
        | Global Tp
        | Unit
        | TpVar TpVar 
        deriving (Eq, Show, Read)
                 
data Tm = Lam Tp TmVar Tm
        | App Tm Tm
        | LamBox Tp TmVar Tm
        | Box Tm
        | TmVar TmVar
        | Const
        deriving (Eq, Show, Read)

typeCheck :: Tm -> Maybe Tp
typeCheck = generate mempty mempty

generate :: M.Map TmVar Tp -> M.Map TmVar Tp -> Tm -> Maybe Tp
generate mpLocal mpGlobal l = case l of 
  Lam a tv e -> do
    b <- generate (M.insert tv a mpLocal) (M.delete tv mpGlobal) e
    return $ a :-> b
  LamBox a tv e -> do
    b <- generate (M.insert tv a mpLocal) (M.insert tv a mpGlobal) e
    return $ (Global a) :-> b
  Box e -> Global <$> generate mpGlobal mpGlobal e
  TmVar tv -> M.lookup tv mpLocal 
  Const -> return Unit
  App e1 e2 -> do
    let gen = generate mpLocal mpGlobal 
    (a :-> b) <- gen e1 
    a' <- gen e2 
    if a == a' then Just b else Nothing