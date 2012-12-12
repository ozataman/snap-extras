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
import           Control.Monad.Trans
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Heist
import           Heist.Interpreted
import           Text.XmlHtml
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Initialize the flash notice system. All you have to do now is to
-- add some flash tags in your application template. See 'flashSplice'
-- for examples.
initFlashNotice 
    :: HasHeist b 
    => SnapletLens b SessionManager -> Initializer b v ()
initFlashNotice session = do
  addSplices [("flash", flashSplice session)]


-------------------------------------------------------------------------------
-- | Display an info message on next load of a page
flashInfo :: SnapletLens b SessionManager -> Text -> Handler b b ()
flashInfo session msg = withSession session $ with session $ setInSession "_info" msg


-------------------------------------------------------------------------------
-- | Display an warning message on next load of a page
flashWarning :: SnapletLens b SessionManager -> Text -> Handler b b ()
flashWarning session msg = withSession session $ with session $ setInSession "_warning" msg


-------------------------------------------------------------------------------
-- | Display a success message on next load of a page
flashSuccess :: SnapletLens b SessionManager -> Text -> Handler b b ()
flashSuccess session msg = withSession session $ with session $ setInSession "_success" msg


-------------------------------------------------------------------------------
-- | Display an error message on next load of a page
flashError :: SnapletLens b SessionManager -> Text -> Handler b b ()
flashError session msg = withSession session $ with session $ setInSession "_error" msg


-------------------------------------------------------------------------------
-- | A splice for rendering a given flash notice dirctive.
--
-- Ex: <flash type='warning'/>
-- Ex: <flash type='success'/>
flashSplice :: SnapletLens b SessionManager -> SnapletISplice b
flashSplice session = do
  typ <- liftM (getAttribute "type") getParamNode
  let typ' = maybe "warning" id typ
  let k = T.concat ["_", typ']
  msg <- lift $ withTop session $ getFromSession k
  case msg of 
    Nothing -> return []
    Just msg' -> do
      lift $ withTop session $ deleteFromSession k >> commitSession
      callTemplateWithText "_flash"
           [ ("type", typ') , ("message", msg') ]

