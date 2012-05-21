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
import Data.Foldable
import Data.Monoid
import Data.Functor
import Data.Map as M
import Data.Set as S
import Debug.Trace

type TpVar = String
type TmVar = String
type Loc = String
type World = S.Set String

data Tp = Tp :-> Tp
        | Tp :@ World
        | Unit
        | TpVar TpVar 
        deriving (Eq, Show, Read)
                 
data Tm = Lam Tp TmVar Tm
        | App Tm Tm
        | LamAt World Tp TmVar Tm
        | BoxAt World Tm
        | TmVar TmVar
        | Const
        deriving (Eq, Show, Read)

typeCheck :: Tm -> Maybe Tp
typeCheck = generate mempty mempty

generate :: M.Map TmVar Tp -> M.Map Loc (M.Map TmVar Tp) -> Tm -> Maybe Tp
generate mpLocal mpGlobal l = case l of 
  Lam a tv e -> do
    b <- generate (M.insert tv a mpLocal) (fmap (M.delete tv) mpGlobal) e
    return $ a :-> b
  LamAt world a tv e -> do
    let addLoc loc mpg = M.alter (Just . maybe (M.singleton tv a) (M.insert tv a)) loc mpg
        mpGlobal' = foldMap addLoc world mpGlobal
    b <- generate (M.insert tv a mpLocal) mpGlobal' e
    return $ a:@world :-> b
  BoxAt world e -> (:@world) <$> generate (trace (show mpGlobal) local) mpGlobal e 
    where local = foldMap (maybe mempty id . (`M.lookup` mpGlobal)) world
  TmVar tv -> M.lookup tv mpLocal 
  Const -> return Unit
  App e1 e2 -> do
    let gen = generate mpLocal mpGlobal 
    (a :-> b) <- gen e1 
    a' <- gen e2 
    if a == a' then Just b else Nothing