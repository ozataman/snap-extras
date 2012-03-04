{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Snap.Extras.FlashNotice
    ( initFlashNotice
    , flashInfo
    , flashWarning
    , flashSuccess
    , flashError
    , flashSplice
    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Data.Lens.Common
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Text.Templating.Heist
import           Text.XmlHtml
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Initialize the flash notice system. All you have to do now is to
-- add some flash tags in your application template. See 'flashSplice'
-- for examples.
initFlashNotice 
    :: HasHeist b 
    => Lens b (Snaplet SessionManager) -> Initializer b v ()
initFlashNotice session = do
  addTemplatesAt "" "resources/templates"
  addSplices [("flash", flashSplice session)]


-------------------------------------------------------------------------------
-- | Display an info message on next load of a page
flashInfo :: Lens b (Snaplet SessionManager) -> Text -> Handler b b ()
flashInfo session msg = withSession session $ with session $ setInSession "_info" msg


-------------------------------------------------------------------------------
-- | Display an warning message on next load of a page
flashWarning :: Lens b (Snaplet SessionManager) -> Text -> Handler b b ()
flashWarning session msg = withSession session $ with session $ setInSession "_warning" msg


-------------------------------------------------------------------------------
-- | Display a success message on next load of a page
flashSuccess :: Lens b (Snaplet SessionManager) -> Text -> Handler b b ()
flashSuccess session msg = withSession session $ with session $ setInSession "_success" msg


-------------------------------------------------------------------------------
-- | Display an error message on next load of a page
flashError :: Lens b (Snaplet SessionManager) -> Text -> Handler b b ()
flashError session msg = withSession session $ with session $ setInSession "_error" msg


-------------------------------------------------------------------------------
-- | A splice for rendering a given flash notice dirctive.
--
-- Ex: <flash type='warning'/>
-- Ex: <flash type='success'/>
flashSplice :: Lens b (Snaplet SessionManager) -> SnapletSplice b v
flashSplice session = do
  typ <- liftHeist $ liftM (getAttribute "type") getParamNode
  let typ' = maybe "warning" id typ
  let k = T.concat ["_", typ']
  msg <- liftHandler $ withTop session $ getFromSession k
  case msg of 
    Nothing -> liftHeist $ return []
    Just msg' -> do
      liftHandler $ withTop session $ deleteFromSession k >> commitSession
      liftHeist $ callTemplateWithText "_flash"
           [ ("type", typ') , ("message", msg') ]

