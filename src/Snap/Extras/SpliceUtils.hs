{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Snap.Extras.SpliceUtils
    ( ifSplice
    , paramSplice
    , utilSplices
    , addUtilSplices
    , selectSplice
    , runTextAreas
    , scriptsSplice
    , ifFlagSplice
    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.Configurator         as C
import qualified Data.Foldable             as F
import           Data.List
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Snap
import           Snap.Snaplet.Heist
import           System.Directory.Tree
import           System.FilePath
import           Heist
import           Heist.Interpreted
import           Text.XmlHtml
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Bind splices offered in this module in your 'Initializer'
addUtilSplices :: HasHeist b => Initializer b v ()
addUtilSplices = addSplices utilSplices


-------------------------------------------------------------------------------
-- | A list of splices offered in this module
utilSplices :: [(Text, SnapletISplice b v)]
utilSplices =
  [ ("rqparam", liftHeist paramSplice)
  ]


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
-- | Assume text are contains the name of a splice as Text.
--
-- This is helpful when you pass a default value to digestive-functors
-- by putting the name of a splice as the value of a textarea tag.
--
-- > heistLocal runTextAreas $ render "joo/index"
runTextAreas :: Monad m => HeistState m -> HeistState m
runTextAreas = bindSplices [ ("textarea", ta)]
 where
   ta = do
     hs <- getHS
     n@(Element t ats _) <- getParamNode
     let nm = nodeText n
     case lookupSplice nm hs of
       Just spl -> do
         ns <- spl
         return [Element t ats ns]
       Nothing -> return $ [Element t ats []]


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
              -- ^ Path to the directory on disk holding the javascript files.
              -> String
              -- ^ A prefix to add to the src attribute of each script tag.
              -> m [Node]
scriptsSplice dir prefix = do
    tree <- liftIO $ build dir
    let files = F.foldMap ((:[]) . fst) $ zipPaths $ "" :/ free tree
        scripts = filter visibleScripts files
    return $ concat $ map includeJavascript scripts
  where
    visibleScripts fname =
        isSuffixOf ".js" fname && not (isPrefixOf "_" (takeFileName fname))
    includeJavascript script =
        [Element "script" [("src", T.pack $ prefix ++ script)] []]



-------------------------------------------------------------------------------
-- | Check to see if the boolean flag named by the "ref" attribute is
-- present and set to true in snaplet user config file. If so, run
-- what's inside this splice, if not, simply omit that part.
--
-- Example:
--
-- > <flag ref="beta-functions-enabled">
-- > stuff...
-- > </flag>
--
-- This will look for an entry inside your .cfg file:
--
-- > beta-functions-enabled = true
ifFlagSplice :: SnapletISplice b v
ifFlagSplice = do
  Element t ats es <- liftHeist getParamNode
  conf <- liftHandler getSnapletUserConfig
  liftHeist $ case lookup "ref" ats of
    Nothing -> return []
    Just flag -> do
      res <- liftIO $ C.lookup conf flag
      case res of
        Just True -> return es
        _         -> return []
