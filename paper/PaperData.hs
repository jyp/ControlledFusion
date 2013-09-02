{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}
module PaperData where

import MarXup
import MarXup.Latex
import MarXup.Latex.Math
import MarXup.Tex
import MarXup.DerivationTrees
import Data.Monoid
import Control.Monad
import Framework

-----------------
-- Preamble 
preamble :: Bool -> Tex ()
preamble inMetaPost = do
  if inMetaPost 
      then documentClass "article" ["9pt"]
      else documentClass "llncs" [] -- ["authoryear","preprint"] 
  stdPreamble
  usepackage "cmll" [] -- for the operator "par"
  mathpreamble LNCS
  cmd "input" (tex "unicodedefs")
  unless inMetaPost $ do
    usepackage "tikz" []
    cmd "usetikzlibrary" $ tex "shapes,arrows"
    usepackage "tabularx" [] 
        
    title "Controlled Fusion"
    authorinfo LNCS authors

    -- cmd "titlebanner" ("Draft of " <> cmd0 "today")
    -- cmd "preprintfooter" "Submitted to POPL 2014"


authors = [AuthorInfo "Jean-Philippe Bernardy" "bernardy@chalmers.se" ch,
           AuthorInfo "Dan Rosén"              "danr@chalmers.se"     ch]
 where ch = "Chalmers University of Technology and University of Gothenburg"
  
