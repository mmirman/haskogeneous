{-# LANGUAGE 
 TemplateHaskell 
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

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals
import System.IO.Unsafe

{-# NOINLINE getQ #-}
getQ = unsafePerformIO . runQ



fromExpToJS (AppE expA expB) = (fromExpToJS expA) ++ " (" ++ fromExpToJS expB ++ ")"
fromExpToJS (VarE exp) = fromNameToJS exp
fromExpToJS (LamE [var] exp) = "function("++ fromPatToJS var ++ "){ return " ++ fromExpToJS exp ++ " }" 
fromExpToJS (LamE [] exp) = "function(){ return " ++ fromExpToJS exp ++ " }" 
fromExpToJS (LamE (var:l) exp) = "function("++ fromPatToJS var ++ "){ return " ++ fromExpToJS (LamE l exp) ++ " }" 
fromExpToJS (LitE lit) = fromLitToJS lit
fromExpToJS (InfixE (Just exp0) (VarE a@(Name (OccName "*") _)) (Just exp1)) = 
  "((" ++ fromExpToJS exp0 ++ ") " ++ fromNameToJS a ++ " (" ++ fromExpToJS exp1 ++ "))"
fromExpToJS (InfixE exp0 expF exp1) = case exp0 of
  Just exp0 -> case exp1 of 
    Just exp1 -> fromExpToJS (AppE (AppE expF exp0) exp1)
fromExpToJS (TupE exps) = fromTupToJS exps
  where fromTupToJS exps = case exps of  
          [] -> ""
          [a] -> fromExpToJS a
          a:l -> fromExpToJS a ++", " ++ fromTupToJS l
fromExpToJS exp = error (show exp)

fromLitToJS (CharL x) = show $ show x
fromLitToJS (StringL x) = show x
fromLitToJS (IntegerL x) = show x
fromLitToJS (RationalL x) = show x
fromLitToJS (IntPrimL x) = show x
fromLitToJS (WordPrimL x) = show x
fromLitToJS (FloatPrimL x) = show x
fromLitToJS (DoublePrimL x) = show x
fromLitToJS (StringPrimL x) = show x

fromNameToJS (Name (OccName a) _) = a

fromPatToJS (VarP a) = fromNameToJS a
fromPatToJS (TupP tp) = case tp of 
  [] -> ""
  [a] -> fromPatToJS a
  a:l -> fromPatToJS a++", "++fromPatToJS (TupP l)

hsToJs = (\x -> "var final = " ++ x ++ ";\n") . fromExpToJS . getQ
alert = undefined

main = do
  putStrLn $ hsToJs [| (\(x,y') -> \y -> y (x * y')) (4,5) alert |]

  