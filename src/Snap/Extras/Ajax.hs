{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Snap.Extras.Ajax
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman
-- Stability   :  experimental
--
-- Simple combinators to work with AJAX requests.
----------------------------------------------------------------------------

module Snap.Extras.Ajax
    ( replaceWith
    , replaceWithTemplate
    , replaceWithTemplateWithSplices
    , jmacroToByteString
    , ResponseType (..)
    , respond
    , responds
    , htmlOrAjax
    , respondAjax
    ) where

-------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad
import           Control.Applicative          as A
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy as BL
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Heist.Compiled
import qualified Heist.Interpreted as I
import           Heist.Splices
import           Heist
import           Language.Javascript.JMacro
import           Safe
import           Snap.Core
import           Snap.Extras.CoreUtils
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Text.XmlHtml as H
-------------------------------------------------------------------------------


-- | Replace innerHTML of given selector with given content.
replaceWith
    :: MonadSnap m
    => Text
    -- ^ jquery selector
    -> ByteString
    -- ^ content blob
    -> m ()
replaceWith sel bs = do
    jsResponse
    writeBS $ jmacroToByteString $ replaceWithJs bs sel


-- | Produce JS statement to replace a target's inner with given
-- contents.
replaceWithJs :: ByteString -> Text -> JStat
replaceWithJs bs sel = [jmacro|
    var contents = `(T.decodeUtf8 bs)`;
    var replaceJs = function() { $(`(sel)`).html(contents); };
    replaceJs();
|]

-- | Converts JMacro-generated Javascript code to a @ByteString@.
jmacroToByteString :: JStat -> ByteString
jmacroToByteString = T.encodeUtf8 . PP.displayTStrict . PP.renderCompact . renderJs

-------------------------------------------------------------------------------
-- | Replace the inner HTML element of a given selector with the
-- contents of the rendered Heist template.
--
-- Currently expect you to have jQuery loaded.
-- TODO: Make this jQuery independent
replaceWithTemplate
    :: HasHeist b
    => ByteString
    -- ^ Heist template name
    -> Text
    -- ^ jQuery selector for target element on page
    -> Handler b v ()
replaceWithTemplate nm sel = do
    (bld, _) <- maybeBadReq "Could not render a response." $
      withHeistState $ \ hs -> renderTemplate hs nm
    bld' <- withTop' id bld
    replaceWith sel (toByteString bld')

-- | Same as @replaceWithTemplate@, but takes additional splices to render the template.
replaceWithTemplateWithSplices
    :: HasHeist b
    => Splices (I.Splice (Handler b b))
    -- ^ bound splices
    -> ByteString
    -- ^ Heist template name
    -> Text
    -- ^ jQuery selector for target element on page
    -> Handler b v ()
replaceWithTemplateWithSplices splices nm sel = do
    doc <- withHeistState $ \ hs -> do
        mb <- I.renderTemplateToDoc (I.bindSplices splices hs) nm
        case mb of
          Nothing -> badReq "Could not render a response." 
          Just doc -> return doc
    bs' <- liftM (BL.toStrict . toLazyByteString . H.renderHtmlFragment H.UTF8 . H.docContent) $ withTop' id doc
    replaceWith sel bs'

-------------------------------------------------------------------------------
-- | Possible reponse types we support at the moment. Can be expanded
-- for more use cases like JSON, CSV and XML.
data ResponseType = Html | Ajax
  deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------
-- | The multi-mime dispatcher. It will inspect the 'Accept' header
-- and determine what type of a request this is. If AJAX, make sure to
-- set the Accept header to 'application/javascript'.
respond :: MonadSnap m => (ResponseType -> m b) -> m b
respond f = do
    hs <- maybeBadReq "Accept header required for this handler" $
          getHeader "accept" A.<$> getRequest
    if B.isInfixOf "application/javascript" hs
      then f Ajax
      else f Html


-- | Dispatch on all response types
responds :: MonadSnap m => [(ResponseType, m b)] -> m b
responds fs = respond $ \ ty -> fromJustNote ("Handler does not know how to respond to: " ++ show ty) (lookup ty fs)


-- | Classic pattern of responding to a static HTML or an alternative
-- AJAX request.
htmlOrAjax
    :: MonadSnap m
    => m b
    -- ^ If call is HTML
    -> m b
    -- ^ If call is AJAX
    -> m b
htmlOrAjax f g = respond $ \ ty -> case ty of
    Html -> f
    Ajax -> g

-- | Responding only to AJAX requests.
respondAjax :: MonadSnap m => m () -> m ()
respondAjax = htmlOrAjax pass
