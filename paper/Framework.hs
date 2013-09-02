{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings #-}

module Framework where

import MarXup
import MarXup.Latex
import MarXup.Tex
import Data.Monoid
import Data.List (intersperse)

return' :: a -> Tex a
return' = return

----------
-- Text
  
anon :: TeX -> TeX
anon x = x -- "(Anonymised) "  
  
specTab x = env "tabularx" $ do 
    braces $ tex "\\textwidth"
    braces $ tex ">{\\centering}X"
    mconcat $ intersperse (newline <> vspace "1ex") x
-- Should replace array
array' :: [String] -> String -> [[TeX]] -> TeX
array' opts format bod = math $ do
  env' "array" opts $ do
    braces $ tex format
    mkrows (map mkcols bod)
  return ()

inferrule :: [TeX] -> TeX -> TeX
inferrule xs y = cmdn "inferrule" [mkrows xs,y] >> return ()

acks = cmd "acks"

comment :: Tex a -> TeX
comment _ = ""

italic :: Tex a -> Tex a
italic = cmd "textit"

figure_ :: TeX -> TeX -> Tex SortedLabel
figure_ caption body = env "figure*" $ do
  body
  cmd "caption" caption
  label "Fig."

mathbox = mbox . math

text = math . cmd "text"

dm = displayMath          

multiline' body = env "multline*" $ mkrows body

space = tex "\\:"

mkIf str = tex "\\newif" <> tex ("\\if" ++ str)

--------------------


