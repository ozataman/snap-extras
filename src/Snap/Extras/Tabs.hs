{-# LANGUAGE OverloadedStrings #-}
{-| 

    Purpose of this module is to provide a simple, functional way to
    define tabs in Snap applications.

-}

module Snap.Extras.Tabs
    ( TabActiveMode (..)
    , Tab
    , mkTabs
    , tab
    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Snap.Core
import           Text.Templating.Heist
import           Text.XmlHtml
import qualified Text.XmlHtml              as X
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- | How do we decide "active" for tab state?
data TabActiveMode
  = TAMExactMatch
  -- ^ Current url has to match exactly
  | TAMPrefixMatch
  -- ^ Only the prefix needs to match current url
  | TAMInfixMatch
  -- ^ A sub-set of the current url has to match


-------------------------------------------------------------------------------
-- | A tab is a 'Node' generator upon receiving a current URL context.
type Tab = Text -> Node


-------------------------------------------------------------------------------
-- | Make tabs from tab definitions. Use the 'tab' combinator to
-- define individual options.
mkTabs 
    :: MonadSnap m 
    => Text 
    -- ^ A class to be given to the parent ul tag
    -> [Tab] 
    -- ^ List of tabs in order
    -> Splice m
mkTabs klass ts = do
  p <- lift $ (T.decodeUtf8 . rqContextPath) `liftM` getRequest
  return [X.Element "ul" [("class", klass)] (map ($ p) ts)]
  

-------------------------------------------------------------------------------
-- | Tab item constructor to be used with 'mkTabs'. Just supply the
-- given arguments here and it will create a 'Tab' ready to be used in
-- 'mkTabs'.
--
-- If the tab is currently active, the li tag will get a class of
-- \'active\'.
tab 
    :: Text 
    -- ^ Target URL for tab
    -> Text 
    -- ^ A text/label for tab
    -> [(Text, Text)] 
    -- ^ A list of attributes as key=val
    -> TabActiveMode 
    -- ^ A 'TabActiveMode' for this tab
    -> Tab
tab url text attr md context = X.Element "li" attr' [link url text]
  where 
    cur = case md of
            TAMExactMatch -> url == T.init context
            TAMPrefixMatch -> url `T.isPrefixOf` context
            TAMInfixMatch -> url `T.isInfixOf` context
    attr' = if cur
            then ("class", klass) : attr
            else attr
    klass = case lookup "class" attr of
              Just k -> T.concat [k, " ", "active"]
              Nothing -> "active"


-------------------------------------------------------------------------------
link :: Text -> Text -> Node
link target text = X.Element "a" [("href", target)] [X.TextNode text]
