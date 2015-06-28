{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8              as S
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                          as T
import           Data.Version                       (showVersion)
import           Network
import           Network.HTTP.Conduit
import           Options.Declarative
import           Text.XmlHtml
import           Text.XmlHtml.Cursor

import           Paths_html2hamlet                  (version)

main :: IO ()
main = run "html2hamlet" (Just $ showVersion version) cmd

cmd :: Arg "FILES/URLS..." [String]
    -> Cmd "HTML to Hamlet converter" ()
cmd (get -> []) = liftIO $
  writeHamlet "stdin" S.getContents S.putStr

cmd (get -> files) = liftIO $ do
  forM_ files $ \file -> do
    if any (`isPrefixOf` file) ["http://", "https://"]
      then do
      writeHamlet file (L.toStrict <$> simpleHttp file) $ S.writeFile (httpFileName file)
      else do
      writeHamlet file (S.readFile file) $ S.writeFile (changeSuffix file)

writeHamlet :: String -> IO S.ByteString -> (S.ByteString -> IO ()) -> IO ()
writeHamlet sourceName reader writer = do
  con <- reader
  let dest = convert sourceName con
  evaluate $ rnf dest
  writer dest

httpFileName :: String -> String
httpFileName url = changeSuffix nsuf
  where
    nsuf | null suf = "index.html"
         | otherwise = suf
    suf = dropQuery $ dropFrag $ reverse $ takeWhile (/='/') $ reverse url
    dropFrag = takeWhile (/='#')
    dropQuery = takeWhile (/='?')

changeSuffix :: String -> String
changeSuffix file
  | any (`isSuffixOf` file) [".html", ".htm"] =
    (++"hamlet") $ reverse $ dropWhile (/='.') $ reverse file
  | otherwise =
    file ++ ".hamlet"

convert :: String -> S.ByteString -> S.ByteString
convert fname content = toByteString $ cvt $ fromNodes nodes
  where
    Right (HtmlDocument enc typ nodes) = parseHTML fname content

    cvt = (fromString "!!!" <>) .
          (<> fromString "\n") .
          go 0

    go lev (Just cur) = slf <> cld <> bro
      where
        slf = single lev (current cur)
        cld = go (lev+1) (firstChild cur)
        bro = go lev (right cur)

    go lev Nothing =
      mempty

    single lev (TextNode txt)
      | T.all isSpace txt = mempty
      | otherwise =
        newline lev <>
        fromText (sanitize txt)
    single lev (Comment comment) =
      mconcat $ do
        line <- T.lines comment
        return $
          newline lev <>
          fromString "$# " <>
          fromText line
    single lev (Element tag attrs _) =
      newline lev <>
      fromString "<" <>
      fromText tag <>
      battr attrs <>
      fromString ">"

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
               T.map (\c -> if c=='\n' then ' ' else c)
