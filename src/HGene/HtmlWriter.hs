{-# LANGUAGE 
 FlexibleInstances, 
 TypeSynonymInstances, 
 NoMonomorphismRestriction,
 TemplateHaskell
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HGene.HtmlWriter
-- License     : http://www.gnu.org/copyleft/gpl.html
-- 
-- Maintainer  : mmirman@andrew.cmu.edu
-- Stability   : experimental
-- Portability : probable
-- 
-- Some tools for writing html in a pretty format in haskell.

-- TODO: Use the module redirect tequnique to exclude items we don't want.
module HGene.HtmlWriter (module X) where 

import HGene.HtmlWriterInternals as X hiding (Printable(..), showMiddle)