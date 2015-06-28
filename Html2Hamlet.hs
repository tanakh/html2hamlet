{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Char
import           Data.List
import qualified Data.Map                     as Map
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Data.Version                 (showVersion)
import           Network.HTTP.Conduit
import           Options.Declarative
import           System.IO
import qualified Text.HTML.DOM                as HTML
import           Text.PrettyPrint.Leijen.Text hiding ((<>))
import           Text.XML
import           Text.XML.Cursor              (child, fromDocument, node)

import           Paths_html2hamlet            (version)

main :: IO ()
main = run "html2hamlet" (Just $ showVersion version) cmd

cmd :: Arg "FILES/URLS..." [String]
    -> Cmd "HTML to Hamlet converter" ()
cmd (get -> []) = liftIO $
  writeHamlet L.getContents putDoc
cmd (get -> files) = liftIO $ do
  forM_ files $ \file -> do
    if any (`isPrefixOf` file) ["http://", "https://"]
      then writeHamlet (simpleHttp file) $ \doc -> withFile (httpFileName file) WriteMode $ \h -> hPutDoc h doc
      else writeHamlet (L.readFile file) $ \doc -> withFile (changeSuffix file) WriteMode $ \h -> hPutDoc h doc

writeHamlet :: IO L.ByteString -> (Doc -> IO ()) -> IO ()
writeHamlet reader writer =
  writer . convert =<< reader

httpFileName :: String -> String
httpFileName url = changeSuffix nsuf
  where
    nsuf | null suf = "index.html"
         | otherwise = suf
    suf = dropQuery $ dropFrag $ reverse $ takeWhile (/= '/') $ reverse url
    dropFrag  = takeWhile (/= '#')
    dropQuery = takeWhile (/= '?')

changeSuffix :: String -> String
changeSuffix file
  | any (`isSuffixOf` file) [".html", ".htm"] =
    (++ "hamlet") $ reverse $ dropWhile (/= '.') $ reverse file
  | otherwise =
    file ++ ".hamlet"

convert :: L.ByteString -> Doc
convert = cvt . fromDocument . HTML.parseLBS where
  cvt doc = "$doctype 5" <$$> go doc
  go cur = fromNode (node cur) <$$> indent 4 (vsep (map go $ child cur))

fromNode :: Node -> Doc
fromNode (NodeElement (Element tag attrs _)) =
  "<" <> hsep (text' (nameLocalName tag): battr attrs) <> ">"
fromNode (NodeContent t    )
  | T.all isSpace t = mempty
  | otherwise       = string $ TL.fromStrict $ T.dropWhile isSpace t
fromNode (NodeComment t    ) = vsep $ map (("$# " <>) . text') $ T.lines t
fromNode (NodeInstruction _) = mempty

battr :: Map.Map Name T.Text -> [Doc]
battr = concatMap (f . first nameLocalName) . Map.toList where
  f ("class", val) = map (("." <>) . text') $ T.words val
  f ("id",    val) = ["#" <> text' val]
  f (key,     val) = [text' key <> "=\"" <> text' val <> "\""]

text' :: T.Text -> Doc
text' = text . TL.fromStrict
