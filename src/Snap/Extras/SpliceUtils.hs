{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Snap.Extras.SpliceUtils
    ( ifSplice
    , paramSplice
    , utilSplices
    , addUtilSplices
    , selectSplice
    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import           Text.XmlHtml
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Bind splices offered in this module in your 'Initializer'
addUtilSplices :: HasHeist b => Initializer b v ()
addUtilSplices = addSplices utilSplices


-------------------------------------------------------------------------------
-- | A list of splices offered in this module
utilSplices :: [(Text, SnapletSplice b v)]
utilSplices = 
  [("rqparam", liftHeist paramSplice)]


-------------------------------------------------------------------------------
-- | Run the splice contents if given condition is True, make splice
-- disappear if not.
ifSplice :: Monad m => Bool -> Splice m
ifSplice cond = 
    case cond of
      False -> return []
      True -> runChildren

------------------------------------------------------------------------------
-- | Gets the value of a request parameter.  Example use:
--
-- <rqparam name="username"/>
paramSplice :: MonadSnap m => Splice m
paramSplice = do
  at <- liftM (getAttribute "name") getParamNode
  val <- case at of
    Just at' -> lift . getParam $ T.encodeUtf8 at'
    Nothing -> return Nothing
  return $ maybe [] ((:[]) . TextNode . T.decodeUtf8) val
    

-------------------------------------------------------------------------------
-- | Splice helper for when you're rendering a select element
selectSplice
    :: Monad m 
    => Text
    -- ^ A name for the select element
    -> Text
    -- ^ An id for the select element
    -> [(Text, Text)]
    -- ^ value, shown text pairs
    -> Maybe Text
    -- ^ Default value
    -> Splice m
selectSplice nm fid xs defv = 
    callTemplate "_select" 
      [("options", opts), ("name", textSplice nm), ("id", textSplice fid)]
    where 
      opts = mapSplices gen xs
      gen (val,txt) = runChildrenWith
        [ ("val", textSplice val)
        , ("text", textSplice txt)
        , ("ifSelected", ifSplice $ maybe False (== val) defv)
        , ("ifNotSelected", ifSplice $ maybe True (/= val) defv) ]