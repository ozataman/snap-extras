{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras
    ( module Snap.Extras.CoreUtils
    , module Snap.Extras.TextUtils
    , module Snap.Extras.JSON
    , module Snap.Extras.FlashNotice
    , module Snap.Extras.SpliceUtils
    , initExtras
    ) where

-------------------------------------------------------------------------------
import Data.Lens.Common
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
-------------------------------------------------------------------------------
import Snap.Extras.CoreUtils
import Snap.Extras.TextUtils
import Snap.Extras.JSON
import Snap.Extras.FlashNotice
import Snap.Extras.SpliceUtils
-------------------------------------------------------------------------------
import Paths_snap_extras
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Initialize all the 'Snap.Extras' functionality in your Snap app
initExtras :: HasHeist b => Lens b (Snaplet SessionManager) -> SnapletInit b ()
initExtras session = 
  makeSnaplet 
    "Snap Extras" 
    "Collection of utilities for web applications" 
    (Just getDataDir) $ do
      initFlashNotice session