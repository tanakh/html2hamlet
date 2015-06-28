{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Arrow
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.Char
import           Data.List
import qualified Data.Map                           as Map
import           Data.Monoid
import qualified Data.Text                          as T
import           Data.Version                       (showVersion)
import           Network.HTTP.Conduit
import           Options.Declarative
import qualified Text.HTML.DOM                      as HTML
import           Text.XML
import           Text.XML.Cursor

import           Paths_html2hamlet                  (version)

main :: IO ()
main = run "html2hamlet" (Just $ showVersion version) cmd

cmd :: Arg "FILES/URLS..." [String]
    -> Cmd "HTML to Hamlet converter" ()
cmd (get -> []) = liftIO $
  writeHamlet L.getContents L.putStr
cmd (get -> files) = liftIO $ do
  forM_ files $ \file -> do
    if any (`isPrefixOf` file) ["http://", "https://"]
      then writeHamlet (simpleHttp file) $ L.writeFile (httpFileName file)
      else writeHamlet (L.readFile file) $ L.writeFile (changeSuffix file)

writeHamlet :: IO L.ByteString -> (L.ByteString -> IO ()) -> IO ()
writeHamlet reader writer = do
  con <- reader
  let dest = convert con
  evaluate $ rnf dest
  writer dest

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

convert :: L.ByteString -> L.ByteString
convert = toLazyByteString . cvt . fromDocument . HTML.parseLBS
  where
    cvt = (fromString "!!!" <>) .
          (<> fromString "\n") .
          go 0

    go lev cur =
      single lev (node cur) <>
      mconcat (map (go $ lev + 1) $ child cur)

    single lev (NodeElement (Element tag attrs _)) =
      newline lev <>
      fromString "<" <>
      fromText (nameLocalName tag) <>
      battr (map (first nameLocalName) $ Map.toList attrs) <>
      fromString ">"

    single lev (NodeContent txt)
      | T.all isSpace txt = mempty
      | otherwise =
        newline lev <>
        fromText (sanitize txt)

    single lev (NodeComment comment) =
      mconcat $ do
        line <- T.lines comment
        return $
          newline lev <>
          fromString "$# " <>
          fromText line

    single _ (NodeInstruction _) =
      mempty

    newline lev =
      fromString "\n" <>
      fromString (replicate (lev*2) ' ')

    battr attrs = mconcat $ map f attrs where
      f ("id", val) =
        fromString " #" <>
        fromText val
      f ("class", val) =
        mconcat $ do
          klass <- T.words val
          return $
            fromString " ." <>
            fromText klass
      f (key, val) =
        fromString " " <>
        fromText key <>
        fromString "=\"" <>
        fromText val <>
        fromString "\""

    sanitize = T.dropWhile isSpace .
               T.map (\c -> if c == '\n' then ' ' else c)
