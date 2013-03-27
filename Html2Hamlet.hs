{-# Language OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}

module Main (main) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network
import Network.HTTP.Conduit hiding (def)
import System.Console.CmdArgs
import Text.XmlHtml
import Text.XmlHtml.Cursor

import qualified Paths_html2hamlet
import Data.Version (showVersion)

data Args =
  Args
  { files :: [String]
  } deriving (Show, Data, Typeable)

main :: IO ()
main = do
  Args files <- cmdArgs $ Args
    { files = def &= args &= typ "FILES/URLS..."
    } &=
    help "HTML to Hamlet converter" &=
    summary ("html2hamlet " ++
             showVersion Paths_html2hamlet.version ++
             " (c) Hideyuki Tanaka 2011")

  if null files
    then do
    con <- B.getContents
    let dest = convert "stdin" con
    B.length dest `seq` B.putStr dest
    else do
    forM_ files $ \file -> do
      if any (`isPrefixOf` file) ["http://", "https://"]
        then withSocketsDo $ do
        let outfile = httpFileName file
        con <- simpleHttp file
        let dest = convert file $ B.concat $ BL.toChunks con
        B.length dest `seq` B.writeFile outfile dest
        else do
        let outfile = changeSuffix file
        con <- B.readFile file
        let dest = convert file con
        B.length dest `seq` B.writeFile outfile dest

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

convert :: String -> B.ByteString -> B.ByteString
convert fname content = toByteString $ cvt $ fromNodes nodes
  where
    Right (HtmlDocument enc typ nodes) = parseHTML fname content

    cvt = (fromString "!!!" `mappend`) .
          (`mappend` fromString "\n") .
          go 0

    go lev (Just cur) = slf `mappend` cld `mappend` bro
      where
        slf = single lev (current cur)
        cld = go (lev+1) (firstChild cur)
        bro = go lev (right cur)

    go lev Nothing =
      mempty

    single lev (TextNode txt)
      | T.all isSpace txt = mempty
      | otherwise =
        newline lev `mappend`
        fromText (sanitize txt)
    single lev (Comment comment) =
      mconcat $ do
        line <- T.lines comment
        return $
          newline lev `mappend`
          fromString "$# " `mappend`
          fromText line
    single lev (Element tag attrs _) =
      newline lev `mappend`
      fromString "<" `mappend`
      fromText tag `mappend`
      battr attrs `mappend`
      fromString ">"

    newline lev =
      fromString "\n" `mappend`
      fromString (replicate (lev*2) ' ')

    battr attrs = mconcat $ map f attrs where
      f ("id", val) =
        fromString " #" `mappend`
        fromText val
      f ("class", val) =
        mconcat $ do
          klass <- T.words val
          return $
            fromString " ." `mappend`
            fromText klass
      f (key, val) =
        fromString " " `mappend`
        fromText key `mappend`
        fromString "=\"" `mappend`
        fromText val `mappend`
        fromString "\""

    sanitize = T.dropWhile isSpace .
               T.map (\c -> if c=='\n' then ' ' else c)
