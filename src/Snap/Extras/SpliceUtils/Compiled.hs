{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras.SpliceUtils.Compiled where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.ByteString
import           Control.Monad.Trans
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Snap.Core
import qualified Snap.Extras.SpliceUtils.Interpreted as I
import           Heist
import           Heist.Compiled
import           Text.XmlHtml
-------------------------------------------------------------------------------


utilSplices :: MonadSnap m => [(Text, Splice m)]
utilSplices = [ ("rqparam", paramSplice)
              , ("refererLink", refererCSplice)
              ]


refererCSplice :: MonadSnap m => Splice m
refererCSplice = return $ yieldRuntimeText $ return .
    maybe "/" T.decodeUtf8 =<< lift (getsRequest (getHeader "Referer"))


------------------------------------------------------------------------------
-- | Gets the value of a request parameter.  Example use:
--
-- <rqparam name="username"/>
paramSplice :: MonadSnap m => Splice m
paramSplice = do
  node <- getParamNode
  let mat = getAttribute "name" node
  case mat of
    Nothing -> error $ (T.unpack $ elementTag node) ++
                       " must have a 'name' attribute"
    Just at -> return $ yieldRuntime $ do
                 val <- lift $ getParam $ T.encodeUtf8 at
                 return $ maybe mempty fromByteString val


------------------------------------------------------------------------------
-- | Searches a directory on disk and all its subdirectories for all files
-- with names that don't begin with an underscore and end with a .js
-- extension.  It then returns script tags for each of these files.
--
-- You can use this function to create a splice:
--
-- > ("staticscripts", scriptsSplice "static/js" "/")
--
-- Then when you use the @\<staticscripts/\>@ tag in your templates, it will
-- automatically include all the javascript code in the @static/js@ directory.
scriptsSplice :: MonadIO m
              => FilePath
                -- ^ Path to the directory on disk holding the javascript
                -- files.
              -> String
                -- ^ A prefix to add to the src attribute of each script tag.
              -> Splice m
scriptsSplice d prefix = runNodeList =<< I.scriptsSplice d prefix


