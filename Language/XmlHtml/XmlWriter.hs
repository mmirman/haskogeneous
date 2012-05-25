{-# LANGUAGE 
 FlexibleInstances, 
 NoMonomorphismRestriction,
 GeneralizedNewtypeDeriving,
 FlexibleContexts,
 EmptyDataDecls,
 MultiParamTypeClasses,
 FunctionalDependencies,
 GADTs
 #-}

---------------------------------------------------------------------------
-- |
-- Module      : HGene.HtmlWriterInternals
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- The xml writer.

-- TODO: Use the module redirect tequnique to exclude items we don't want.
module Language.XmlHtml.XmlWriter where
       
import Control.Monad.Trans
import Control.Monad.Writer.Strict (WriterT, tell, execWriterT)
import Control.Monad.Writer.Class (MonadWriter(..))

void a = a >> return ()

-- --------------------------------------------------------------------------
-- Type definition for the HtmlWriter
newtype HtmlWriter a = HtmlWriter { getHtml :: WriterT String IO a}
                     deriving (Monad, MonadWriter String)

liftIO t = HtmlWriter $ lift t

-- | @'writeString' s@ writes a string to the html writer monad
writeString = tell

-- | @'makeHtml' s@ currently just converts this into a string
makeXml = execWriterT . getHtml . printThis

data End
data Par

data Param a b where 
  Param :: Printable a b => [HtmlAttr] -> a -> Param a b

data Elem a b where 
  Elem :: Printable a b => String -> a -> Elem a b

data HtmlAttr = Attr String String

instance Show HtmlAttr where
  show (Attr a b) = a++"="++b

-- --------------------------------------------------------------------------
-- Printable allows us to use tag for either a monad or a string or whatever
-- just makes syntax better, and ideally in the future, everything better.
class Printable a b | a -> b where printThis :: a -> HtmlWriter ()

instance Printable [Char] End where
  printThis = writeString

instance Printable (HtmlWriter a) End where 
  printThis = void

instance Printable a Par => Printable (Elem a Par) End where
  printThis (Elem tg msg) = do  
    writeString $ "<"++tg
    printThis msg
    writeString $ "</"++tg++">\n"

instance Printable a End => Printable (Elem a End) End where 
  printThis (Elem tg msg) = do
    writeString $ "<"++tg++">\n"
    printThis msg
    writeString $ "</"++tg++">\n"
    
instance Printable a Par => Printable (Param a Par) Par where
  printThis (Param params msg) = do
    printThis $ showMiddle params
    printThis msg
    
instance Printable a End => Printable (Param a End) Par where
  printThis (Param params msg) = do
    printThis $ showMiddle params++">\n"
    printThis msg  
  
showMiddle = concatMap (\x -> " "++show x)

tag tg = printThis . Elem tg
