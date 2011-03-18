{-# Language OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}

module Main (main) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
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
      let outfile =
            if any (`isSuffixOf` file) [".html", ".htm"]
            then (++"hamlet") $ reverse $ dropWhile (/='.') $ reverse file
            else file ++ ".hamlet"
      con <- B.readFile file
      let dest = convert file con
      B.length dest `seq` B.writeFile outfile dest

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
        fromString "\n" `mappend`
        fromString (replicate (lev*2) ' ') `mappend`
        fromText (sanitize txt)
    single lev (Comment comment) = mempty
    single lev (Element tag attrs _) =
      fromString "\n" `mappend`
      fromString (replicate (lev*2) ' ') `mappend`
      fromString "<" `mappend`
      fromText tag `mappend`
      battr attrs `mappend`
      fromString ">"
    
    battr attrs = mconcat $ map f attrs where
      f ("id", val) =
        fromString " #" `mappend`
        fromText val
      f ("class", val) =
        fromString " ." `mappend`
        fromText val
      f (key, val) =
        fromString " " `mappend`
        fromText key `mappend`
        fromString "=\"" `mappend`
        fromText val `mappend`
        fromString "\""

    sanitize = T.dropWhile isSpace .
               T.map (\c -> if c=='\n' then ' ' else c)
