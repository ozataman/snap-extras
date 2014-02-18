{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras.SpliceUtils.Compiled where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.ByteString
import           Control.Monad
import           Control.Monad.Trans
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Snap.Core
import qualified Snap.Extras.SpliceUtils.Interpreted as I
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import           Text.XmlHtml
-------------------------------------------------------------------------------


utilSplices :: MonadSnap m => Splices (Splice m)
utilSplices = do
    "rqparam" ## paramSplice
    "refererLink" ## refererCSplice


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


------------------------------------------------------------------------------
-- | Sometimes in a loop you don't always want the same behavior for every
-- item.  If you have a comma separated list, you usually don't want a comma
-- after the last item.  If you have a list surrounded by parentheses, you
-- might not want the parentheses to show up if the list is empty.  Dealing
-- with these situations can be a pain with the stock looping splices, so
-- we've provided this helper that solves all of these problems.
--
-- This function is similar to manyWithSplices, but it binds two additional
-- splices: \"whenNonempty\" and \"whenMultiple\".  The former only shows its
-- children when the list has 1 or more elements.  The latter only shows its
-- children when the list has more than 1 element.
fancyLoopSplice :: Monad n
                => Splices (RuntimeSplice n a -> Splice n)
                -> RuntimeSplice n [a]
                -> Splice n
fancyLoopSplice splices action = do
    p <- newEmptyPromise
    q <- newEmptyPromise
    let splices' = do
          mapS ($ getPromise q) splices
          "whenNonempty" ## checkPred (> 0) p
          "whenMultiple" ## checkPred (> 1) p

    chunks <- withLocalSplices splices' noSplices runChildren
    return $ yieldRuntime $ do
        items <- action
        putPromise p $ length items
        res <- forM items $ \item -> putPromise q item >> codeGen chunks
        return $ mconcat res
  where
    checkPred predicate p = do
        chunks <- runChildren
        return $ yieldRuntime $ do
            len <- getPromise p
            if predicate len then codeGen chunks else return mempty

