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

{-# NOINLINE getQ #-}
getQ = unsafePerformIO . runQ

jsFlip = toJS $ getQ [| (\f x y -> f y x) |]

-- Mangling with "VALUE" issues
lazyValue str = "function (){\n\
                \  var used  = 0;\n\
                \  var value = 0;\n\
                \  return function(){\n\ 
                \     if (!used) {\n\
                \        value = "++str++";\n\
                \        used = 1;\n\
                \     }\n\
                \     return value;\n\
                \  };\n\
                \}()"

toJS_RHS = lazyValue . toJS

class Convertable a where
  toJS :: a -> String

instance Convertable Exp where
  toJS (AppE expA expB) = (toJS expA) ++ " (" ++ toJS_RHS expB ++ ")"
  toJS (VarE exp) = toJS exp
  toJS (LamE [] exp) = "function(){ return " ++ toJS exp ++ " }" 
  toJS (LamE [var] exp) = "function("++ toJS var ++ "){ return " ++ toJS exp ++ " }" 
  toJS (LamE (var:l) exp) = "function("++ toJS var ++ "){ return " ++ toJS (LamE l exp) ++ " }" 
  toJS (LitE lit) = toJS lit
  toJS (InfixE (Just exp0) (VarE a@(Name (OccName "*") _)) (Just exp1)) = 
    "((" ++ toJS exp0 ++ ") " ++ toJS a ++ " (" ++ toJS exp1 ++ "))"
  toJS (InfixE exp0 expF exp1) = case exp0 of
    Just exp0 -> case exp1 of 
      Just exp1 -> toJS (AppE (AppE expF exp0) exp1)
      Nothing -> toJS (AppE expF exp0)
    Nothing -> case exp1 of 
      Just exp1 -> "(" ++ jsFlip ++ " (" ++ toJS expF ++ "))("++toJS_RHS exp1 ++")"
  toJS (TupE exps) = fromTupToJS exps
    where fromTupToJS exps = case exps of  
            [] -> ""
            [a] -> toJS a
            a:l -> toJS a ++", " ++ fromTupToJS l
  toJS exp = error (show exp)

instance Convertable Lit where
  toJS (CharL x) = show $ show x
  toJS (StringL x) = show x
  toJS (IntegerL x) = show x
  toJS (RationalL x) = show x
  toJS (IntPrimL x) = show x
  toJS (WordPrimL x) = show x
  toJS (FloatPrimL x) = show x
  toJS (DoublePrimL x) = show x
  toJS (StringPrimL x) = show x

deriving instance Show NameFlavour
deriving instance Show NameSpace
deriving instance Show PkgName
deriving instance Show ModName

instance Convertable Name where
  toJS (Name (OccName a) (NameG _ _ (ModName "HGene.JSCompiler.JSBase"))) = a
  toJS arg = (show arg)++"()"
  
instance Convertable Pat where
  toJS (VarP a) = show a
  toJS (TupP tp) = case tp of 
    [] -> ""
    [a] -> toJS a
    a:l -> toJS a++", "++toJS (TupP l)

-- Mangling with "FINAL" issues
hsToJs :: Q Exp -> [Char]
hsToJs = (\x -> "var final = " ++ x ++ ";\n") . toJS . getQ

test = hsToJs [| (\x -> x) (alert 5) |]