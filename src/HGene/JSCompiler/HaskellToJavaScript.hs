{-# LANGUAGE 
 TemplateHaskell,
 StandaloneDeriving
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      : HGene.JSCompiler.HaskellToJavaScript
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- Some tools for writing html in a pretty format in haskell.

module HGene.JSCompiler.HaskellToJavaScript (hsToJs) where

import HGene.JSCompiler.JSBase
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals
import System.IO.Unsafe

jsFlip = hsToJsStr [| (\f x y -> f y x) |]

getNewName str = do
  n <- newName str
  return $ show n ++ "_v"
  
-- Mangling with "VALUE" issues
lazyValue str = do
  used <- getNewName "used"
  value <- getNewName "value"
  return $ "function (){\n\
           \  var "++used++"  = 0;\n\
           \  var "++value++" = 0;\n\
           \  return function(){\n\ 
           \     if (!"++used++") {\n\
           \        "++value++" = ("++str++");\n\
           \        "++used++" = 1;\n\
           \     }\n\
           \     return "++value++";\n\
           \  };\n\
           \}()"

toJS_RHS e = do 
 a <- toJS e
 lazyValue a

class Convertable a where
  toJS :: a -> Q String

instance Convertable Exp where
  toJS (AppE expA expB) = do
    a <- toJS expA 
    b <- toJS_RHS expB
    return $ a++" (" ++ b ++ ")"
  toJS (VarE exp) = toJS exp
  toJS (LamE [] exp) = do
    e <- toJS exp
    return $ "function(){ return " ++ e ++ " }" 
  toJS (LamE [var] exp) = do 
    v <- toJS var
    e <- toJS exp
    return $ "function("++ v ++ "){ return " ++ e ++ " }" 
  toJS (LamE (var:l) exp) = do
    v <- toJS var
    e <- toJS $ LamE l exp
    return $ "function("++ v ++ "){ return " ++ e ++ " }" 
  toJS (LitE lit) = toJS lit
  toJS (InfixE (Just exp0) (VarE (Name (OccName "*") _)) (Just exp1)) = do
    e0 <- toJS exp0
    e1 <- toJS exp1
    return $ "((" ++ e0 ++ ") * (" ++ e1 ++ "))"
  toJS (InfixE exp0 expF exp1) = case exp0 of
    Just exp0 -> case exp1 of 
      Just exp1 -> toJS (AppE (AppE expF exp0) exp1)
      Nothing -> toJS (AppE expF exp0)
    Nothing -> case exp1 of 
      Just exp1 -> do
        ef <- toJS expF
        e1 <- toJS_RHS exp1
        flp <- jsFlip
        return $ "(" ++ flp ++ " (" ++ ef ++ "))("++e1 ++")"
  toJS (TupE exps) = fromTupToJS exps
    where fromTupToJS exps = case exps of  
            [] -> return ""
            [a] -> toJS a
            a:l -> do
              a' <- toJS a 
              l' <- fromTupToJS l
              return $ a' ++", " ++ l'
  toJS (DoE stmts) = do 
      eid <- [| \a -> a |]
      noBind <- [| (>>) |]
      bind <- [| (>>=) |]
      let convertDo st = case st of
            BindS v e -> AppE (AppE bind e) . LamE [v]
            NoBindS e -> AppE (AppE noBind e)
            LetS decs -> LetE decs
      toJS $ foldr (.) id (map convertDo stmts) eid
      
  toJS exp = error (show exp)
  
instance Convertable Stmt where
  

instance Convertable Lit where
  toJS (CharL x) = return $ show $ show x
  toJS (StringL x) = return $ show x
  toJS (IntegerL x) = return $ show x
  toJS (RationalL x) = return $ show x
  toJS (IntPrimL x) = return $ show x
  toJS (WordPrimL x) = return $ show x
  toJS (FloatPrimL x) = return $ show x
  toJS (DoublePrimL x) = return $ show x
  toJS (StringPrimL x) = return $ show x

deriving instance Show NameFlavour
deriving instance Show NameSpace
deriving instance Show PkgName
deriving instance Show ModName

--reify :: Name -> Q Info

instance Convertable Name where
  toJS n@(Name (OccName a) (NameG _ _ (ModName "HGene.JSCompiler.JSBase"))) = do
    VarI _ tp _ _ <- qReify n
    let strictify (ForallT _ _ t) f = strictify t f
        strictify (AppT (AppT ArrowT _) t) f = do
          i <- getNewName "arg"
          s <- strictify t (f++"("++i++"())")
          return $ "function("++i++"){"++s++"}"
        strictify _ f = return f
    strictify tp a 
  toJS arg = return $ (show arg)++"()"
  
instance Convertable Pat where
  toJS (VarP a) = return $ show a
  toJS (TupP tp) = case tp of 
    [] -> return ""
    [a] -> toJS a
    a:l -> do
      a' <- toJS a
      l' <- toJS (TupP l)
      return $ a'++", "++l'

-- Mangling with "FINAL" issues
hsToJsStr :: Q Exp -> Q String
hsToJsStr a = do
  a' <- a
  x <- toJS a'
  return $ "var final = " ++ x ++ ";\n"
  
hsToJs h = do
  h' <- hsToJsStr h
  return $ LitE $ StringL h'
  
test = hsToJs [| (\x -> x) (alert 5) |]
