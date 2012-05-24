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

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.Foldable hiding (foldr)
import Data.Monoid
import Data.Functor
import Data.Map as M
import Data.Set as S
import Debug.Trace

import Data.Unique

data Var = Lit String 
         | Gen Int
         deriving (Eq, Ord, Show, Read)

type TpVar = Var
type TmVar = String
type Loc = String
type World = S.Set String

data Tp = Tp :-> Tp
        | Tp :@ World
        | Unit
        | TpVar TpVar 
        | Forall TpVar Tp
        deriving (Eq, Show, Read)
                 
data Tm = Lam Tp TmVar Tm
        | App Tm Tm
        | LamAt World Tp TmVar Tm
        | BoxAt World Tm
        | TmVar TmVar
        | Const
        | TyLam TpVar Tm          
        | TyApp Tm Tp
        deriving (Eq, Show, Read)


typeCheck :: Tm -> MaybeT IO Tp
typeCheck = generate mempty mempty

substitute :: TpVar -> Tp -> Tp -> Tp
substitute tO tN t = case t of
  Forall tO' _ | tO' == tO -> t
  Forall tO' e  -> Forall tO' $ substitute tO tN e
  TpVar tO' | tO' == tO -> tN
  TpVar tO' -> TpVar tO'
  Unit -> Unit
  t1 :-> t2 -> substitute tO tN t1 :-> substitute tO tN t2
  t1 :@ w -> substitute tO tN t1 :@ w


regen :: Tp -> MaybeT IO Tp
regen (Forall tO t) = do
  u <- Gen <$> hashUnique <$> lift newUnique 
  return $ Forall u $ substitute tO (TpVar u) t
regen (t :@ w) = (:@ w) <$> regen t
regen (t1 :-> t2) = do
  t1' <- regen t1 
  t2' <- regen t2
  return $ t1' :-> t2'
regen t = return t

returnMaybe = MaybeT . return

generate :: M.Map TmVar Tp -> M.Map Loc (M.Map TmVar Tp) -> Tm -> MaybeT IO Tp
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
    where local = foldr (M.intersection . get) (get a) (a:l)
          get = maybe mempty id . (`M.lookup` mpGlobal)
          (a:l) = (S.toList world)
  TmVar tv -> returnMaybe $ M.lookup tv mpLocal 
  Const -> return Unit
  App e1 e2 -> do
    let gen = generate mpLocal mpGlobal 
    (a :-> b) <- gen e1 
    a' <- gen e2 
    returnMaybe $ if a == a' then Just b else Nothing
    
  TyLam ty e -> do
    b <- generate mpLocal mpGlobal e
    return $ Forall ty b
  
  TyApp e ty -> do
    Forall tv b <- generate mpLocal mpGlobal e
    ty' <- regen ty
    return $ substitute tv ty' b


