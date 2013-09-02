{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

module Paper where

import MarXup
import MarXup.Latex
import MarXup.Tex
import Data.Monoid
import Framework
import Data.List
import PaperData

acmCategories,keywords,abstract,header :: TeX
acmCategories = do
  cmdn_ "category" ["F.4.1","Mathematical Logic","Lambda calculus and related system"]
  cmdn_ "category" ["D.3.3","Language Constructs and Features","Concurrent programming structures"]
  
keywords = do  
  cmd0 "keywords"
  mconcat $ intersperse ", " ["deforestation","fusion", "..."]

abstract = env "abstract" «»

header = do
  maketitle
  abstract
  keywords
  
outputTexMp :: Bool -> MPOutFormat -> String -> IO ()
outputTexMp includeAppendix fmt name = renderToDisk' fmt name $ latexDocument preamble «

@header

arstras arstarstarst arst arsta rst
 tdiqlwfjhpd;wpdh 
             
@(emph «rstarst»)

@intro
               
@intro<-section«Introduction»
  
@section«Conclusion»



»
