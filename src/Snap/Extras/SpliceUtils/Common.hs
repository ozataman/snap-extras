module Snap.Extras.SpliceUtils.Common where

-------------------------------------------------------------------------------
import           Control.Monad.Trans
import qualified Data.Foldable         as F
import           Data.List
import           System.Directory.Tree
import           System.FilePath
-------------------------------------------------------------------------------

getScripts :: MonadIO m => FilePath -> m [String]
getScripts d = do
    tree <- liftIO $ build d
    let files = F.foldMap ((:[]) . fst) $ zipPaths $ "" :/ dirTree tree
    return $ sort $ filter visibleScripts files
  where
    visibleScripts fname =
      isSuffixOf ".js" fname && not ("_" `isPrefixOf` takeFileName fname)

