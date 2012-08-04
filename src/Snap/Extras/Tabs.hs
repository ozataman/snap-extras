{-# LANGUAGE OverloadedStrings #-}
{-| 

    Purpose of this module is to provide a simple, functional way to
    define tabs in Snap applications.

-}

module Snap.Extras.Tabs
    ( 
    -- * Define Tabs in DOM via Heist
      initTabs
    , tabsSplice
    
    -- * Define Tabs in Haskell
    , TabActiveMode (..)
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
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import           Text.XmlHtml
import qualified Text.XmlHtml              as X
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
initTabs :: HasHeist b => Initializer b v ()
initTabs = do
  addSplices [ ("tabs", liftHeist tabsSplice) ]


                              -------------------
                              -- Splice-Driven --
                              -------------------


-------------------------------------------------------------------------------
tabsSplice :: MonadSnap m => Splice m
tabsSplice = do
  context <- lift $ (T.decodeUtf8 . rqContextPath) `liftM` getRequest
  let bind = bindSplices [("tab", tabSplice context)]
  n <- getParamNode
  case n of
    Element t attrs ch -> localTS bind $ runNodeList [X.Element "ul" attrs ch]
    _ -> error "tabs tag has to be an Element"



-------------------------------------------------------------------------------
tabSplice :: MonadSnap m => Text -> Splice m
tabSplice context = do
  n@(Element t attrs ch) <- getParamNode
  let ps = do
        m <- wErr "tab must specify a 'match' attribute" $ lookup "match" attrs
        url <- wErr "tabs must specify a 'url' attribute" $ getAttribute "url" n
        m' <- case m of
          "Exact" -> Right $ url == context
          "Prefix" -> Right $ url `T.isPrefixOf` context
          "Infix" -> Right $ url `T.isInfixOf` context
          "None" -> Right $ False
          _ -> Left "Unknown match type"
        ch <- return $ childNodes n
        return (url, ch, m')
  case ps of
    Left e -> error $ "Tab error: " ++ e
    Right (url, ch, match) -> do
      let attr' = if match then ("class", "active") : attrs else attrs
          a = X.Element "a" (("href", url) : attrs) ch
      return $ [X.Element "li" attr' [a]]


-------------------------------------------------------------------------------
wErr err m = maybe (Left err) Right m


                             --------------------
                             -- Haskell-Driven --
                             --------------------





-------------------------------------------------------------------------------
-- | How do we decide "active" for tab state?
data TabActiveMode
  = TAMExactMatch
  -- ^ Current url has to match exactly
  | TAMPrefixMatch
  -- ^ Only the prefix needs to match current url
  | TAMInfixMatch
  -- ^ A sub-set of the current url has to match
  | TAMDontMatch


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
--
-- Make sure to provide a trailing / when indicating URLs as snap
-- context paths contain it and active tab checks will be confused
-- without it.
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
tab url text attr md context = X.Element "li" attr' [tlink url text]
  where 
    cur = case md of
            TAMExactMatch -> url == context
            TAMPrefixMatch -> url `T.isPrefixOf` context
            TAMInfixMatch -> url `T.isInfixOf` context
            TAMDontMatch -> False
    attr' = if cur
            then ("class", klass) : attr
            else attr
    klass = case lookup "class" attr of
              Just k -> T.concat [k, " ", "active"]
              Nothing -> "active"


-------------------------------------------------------------------------------
tlink :: Text -> Text -> Node
tlink target text = X.Element "a" [("href", target)] [X.TextNode text]


-------------------------------------------------------------------------------
link :: Text -> [Node] -> Node
link target ch = X.Element "a" [("href", target)] ch
