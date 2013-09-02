module Main (main) where

import Cake hiding (system)
import Cake.Tex
import Cake.MarXup
import MarXup.Tex
import Paper
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import System.Directory (doesFileExist)
import Control.Monad.Trans (liftIO)
import System.Process (system)
import Data.List (intercalate)

importExternalFile :: FilePath -> Act ()
importExternalFile what = do
  e <- liftIO $ doesFileExist what
  when e $ liftIO $ do hPutStrLn stderr $ "refreshing " ++ what
                       system $ intercalate " " ["cp",what,"."]
                       return ()

main :: IO ()
main = do
  outputTexMp True EPS name
  cake empty $ do 
    importExternalFile "../gitroot/bibtex/jp.bib"
    importExternalFile "../gitroot/latex/unicodedefs.tex"
    _pdflatex name

mainSansAppendix :: IO ()
mainSansAppendix = do
  outputTexMp False EPS name
  cake empty $ do 
    importExternalFile "../gitroot/bibtex/jp.bib"
    importExternalFile "../gitroot/latex/unicodedefs.tex"
    pdflatexMarxup name


name :: String  
name = "paper"
