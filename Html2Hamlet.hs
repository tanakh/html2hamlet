{-# Language OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}

module Main (main) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Char
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import System.Console.CmdArgs
import Text.XmlHtml
import Text.XmlHtml.Cursor

data Args =
  Args
  { files :: [String]
  } deriving (Show, Data, Typeable)

main :: IO ()
main = do
  Args files <- cmdArgs $ Args
    { files = [] &= args &= typFile
    } &= summary "HTML to Hamler converter"
  con <- B.getContents
  B.putStrLn $ convert "<stdin>" con
  return ()

convert :: String -> B.ByteString -> B.ByteString
convert fname content = toByteString $ go 0 $ fromNodes nodes
  where
    Right (HtmlDocument enc typ nodes) = parseHTML fname content
    
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

    sanitize = T.map (\c -> if c=='\n' then ' ' else c)
