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
    , tabsCSplice

    -- * Define Tabs in Haskell
    , TabActiveMode (..)
    , Tab
    , mkTabs
    , tab
    ) where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Class
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled            as C
import           Heist.Interpreted
import           Text.XmlHtml
import qualified Text.XmlHtml              as X
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
initTabs :: HasHeist b => Snaplet (Heist b) -> Initializer b v ()
initTabs h = do
    let splices = [ ("tabs", tabsSplice) ]
        csplices = [ ("tabs", tabsCSplice) ]
    addConfig h $ mempty { hcCompiledSplices = csplices
                         , hcInterpretedSplices = splices }


                              -------------------
                              -- Splice-Driven --
                              -------------------


-------------------------------------------------------------------------------
-- | Compiled splice for tabs.
tabsCSplice :: MonadSnap m => C.Splice m
tabsCSplice = do
    n <- getParamNode
    let getContext = lift $ (T.decodeUtf8 . rqURI) `liftM` getRequest
        splices = [("tab", C.defer tabCSplice getContext)]
    case n of
      Element t attrs ch -> C.withLocalSplices splices [] $
          C.runNode $ X.Element "ul" attrs ch
      _ -> error "tabs tag has to be an Element"


------------------------------------------------------------------------------
-- | Can't use tabSpliceWorker because we have to explicitly run the
-- attributes in order to get ${} splice substitution.
tabCSplice :: Monad m => C.Promise Text -> C.Splice m
tabCSplice promise = do
    n@(Element _ attrs ch) <- getParamNode
    attrsAction <- C.runAttributesRaw attrs
    let ps as context = do
          m <- wErr "tab must specify a 'match' attribute" $ lookup "match" as
          url <- wErr "tabs must specify a 'url' attribute" $ lookup "url" as
          m' <- case m of
            "Exact" -> Right $ url == context
            "Prefix" -> Right $ url `T.isPrefixOf` context
            "Infix" -> Right $ url `T.isInfixOf` context
            "None" -> Right $ False
            _ -> Left "Unknown match type"
          return (url, ch, m')
    return $ C.yieldRuntime $ do
        ctx <- C.getPromise promise
        as <- attrsAction
        let res = case ps as ctx of
              Left e -> error $ "Tab error: " ++ e
              Right (url, ch, match) ->
                let attr' = if match then ("class", "active") : as else as
                    a = X.Element "a" (("href", url) : as) ch
                 in X.renderHtmlFragment X.UTF8 [X.Element "li" attr' [a]]
        return res


tabSpliceWorker :: Node -> Text -> [Node]
tabSpliceWorker n@(Element _ attrs ch) context =
    case ps of
      Left e -> error $ "Tab error: " ++ e
      Right (url, ch, match) ->
        let attr' = if match then ("class", "active") : attrs else attrs
            a = X.Element "a" (("href", url) : attrs) ch
         in [X.Element "li" attr' [a]]
  where
    ps = do
      m <- wErr "tab must specify a 'match' attribute" $ lookup "match" attrs
      url <- wErr "tabs must specify a 'url' attribute" $ getAttribute "url" n
      m' <- case m of
        "Exact" -> Right $ url == context
        "Prefix" -> Right $ url `T.isPrefixOf` context
        "Infix" -> Right $ url `T.isInfixOf` context
        "None" -> Right $ False
        _ -> Left "Unknown match type"
      return (url, ch, m')


-------------------------------------------------------------------------------
tabsSplice :: MonadSnap m => Splice m
tabsSplice = do
  context <- lift $ (T.decodeUtf8 . rqURI) `liftM` getRequest
  let bind = bindSplices [("tab", tabSplice context)]
  n <- getParamNode
  case n of
    Element t attrs ch -> localHS bind $ runNodeList [X.Element "ul" attrs ch]
    _ -> error "tabs tag has to be an Element"



-------------------------------------------------------------------------------
tabSplice :: Monad m => Text -> HeistT n m [Node]
tabSplice context = do
  n <- getParamNode
  return $ tabSpliceWorker n context


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
