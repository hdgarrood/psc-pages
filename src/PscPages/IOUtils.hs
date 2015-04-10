module PscPages.IOUtils where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)

writeLazyTextFile :: FilePath -> TL.Text -> IO ()
writeLazyTextFile fp t = BL.writeFile fp (TLE.encodeUtf8 t)

writeTextFile :: FilePath -> T.Text -> IO ()
writeTextFile fp t = B.writeFile fp (TE.encodeUtf8 t)

readLazyTextFile :: FilePath -> IO TL.Text
readLazyTextFile fp = fmap TLE.decodeUtf8 (BL.readFile fp)

readTextFile :: FilePath -> IO T.Text
readTextFile fp = fmap TE.decodeUtf8 (B.readFile fp)

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True

mkParentDir :: FilePath -> IO ()
mkParentDir = mkdirp . takeDirectory
