{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Snap.Extras.NavTrails where

import           Blaze.ByteString.Builder.ByteString
import           Control.Monad.State.Strict
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Heist
import qualified Heist.Compiled as C
import           Heist.Interpreted



-------------------------------------------------------------------------------
data NavTrail b = NavTrail {
      ntSes :: SnapletLens b SessionManager
      -- ^ A session manager for the base
    }


-------------------------------------------------------------------------------
--initNavTrail
--    :: HasHeist b
--    => SnapletLens b SessionManager
--    -> Bool
--    -- ^ Auto-add all splices?
--    -> SnapletInit b (NavTrail b)
initNavTrail :: SnapletLens b SessionManager
             -- ^ Lens to the session snaplet
             -> Maybe (Snaplet (Heist b))
             -- ^ The heist snaplet (not a lens), if you want splices to be
             -- added automatically.
             -> SnapletInit b (NavTrail b)
initNavTrail ses heist =
  makeSnaplet "NavTrail"
              "Makes it easier for you to navigate back to key app points."
              Nothing $ do
  maybe (return ()) addNavTrailSplices heist
  return $ NavTrail ses


-------------------------------------------------------------------------------
-- |
setFocus = do
  setFocus' =<< rqURI `fmap` getRequest


-------------------------------------------------------------------------------
-- |
setFocus' uri = do
  sl <- gets ntSes
  withSession sl $ withTop sl $ do
    setInSession "_nt_focus" $ T.decodeUtf8 uri


-------------------------------------------------------------------------------
-- |
setFocusToRef = do
  sl <- gets ntSes
  (maybe "/" id . getHeader "Referer") `fmap` getRequest >>=
    withTop sl . setInSession "_nt_focus" . T.decodeUtf8


-------------------------------------------------------------------------------
-- |
getFocus = do
  sl <- gets ntSes
  withTop sl (getFromSession "_nt_focus")


getFocusDef def = (fromJust . (`mplus` Just def)) `fmap` getFocus



-------------------------------------------------------------------------------
-- |
redirBack = redirect =<< (maybe "/" id . getHeader "Referer") `fmap` getRequest


-------------------------------------------------------------------------------
-- |
redirFocus def = do
  f <- (`mplus` Just def) `fmap` (fmap T.encodeUtf8 `fmap` getFocus)
  redirect $ fromJust f


-------------------------------------------------------------------------------
-- |
backSplice :: MonadSnap m => HeistT m m Template
backSplice = do
  f <- rqURI `fmap` getRequest
  textSplice $ T.decodeUtf8 f

backCSplice :: C.Splice (Handler b v)
backCSplice = return $ C.yieldRuntime $ do
  lift $ (fromByteString . rqURI) `fmap` getRequest

-------------------------------------------------------------------------------
-- |
focusSplice :: SnapletLens (Snaplet v) (NavTrail b)
            -> Splice (Handler b v)
focusSplice lens = do
  uri <- lift $ with' lens getFocus
  maybe (return []) textSplice uri

focusCSplice :: SnapletLens (Snaplet v) (NavTrail b)
             -> C.Splice (Handler b v)
focusCSplice lens = return $ C.yieldRuntimeText $ do
  uri <- lift $ with' lens getFocus
  return $ fromMaybe "" uri

-------------------------------------------------------------------------------
-- |
addNavTrailSplices heist = do
  lens <- getLens
  addConfig heist $
    mempty { hcCompiledSplices =
               [ ("linkToFocus", focusCSplice lens)
               , ("linkToBack", backCSplice) ]
           , hcInterpretedSplices =
               [ ("linkToFocus", focusSplice lens)
               , ("linkToBack", backSplice) ]
           }


