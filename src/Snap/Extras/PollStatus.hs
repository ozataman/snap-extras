{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

{-|

This module provides infrastructure for polling the status of processes that
run longer than a single request-response lifecycle.  To do this, we issue
AJAX calls at regular intervals to a route that updates the status on the
page.  There are two main components necessary to use this library: splices
and a handler.

The handler is a simple 'jobStatusHandler' function.  Typically you'll add it
to your site's list of routes like this:

> route [ ...
>       , ("myJobStatus", jobStatusHandler "statusTemplate" ".statusDiv")
>       , ...
>       ]

The first argument to jobsHandler is the template that will be rendered for
status update requests.  It will typically be not much more than a
@\<myJobStatus\>@ tag enclosing the custom markup for displaying your job status.
Here's an example using a bootstrap progress bar:

> <myJobStatus interval="300">
>   <ifRunning>
>     <elapsedSeconds/> seconds elapsed
>
>     <div class="progress">
>       <div class="progress-bar progress-bar-striped active"  role="progressbar"
>            aria-valuenow="${amountCompleted}" aria-valuemin="0"
>            aria-valuemax="${amountTotal}" style="width: ${percentCompleted}%">
>         <percentCompleted/>%
>       </div>
>     </div>
>
>   </ifRunning>
>   <ifFinished>
>     <div class="progress">
>       <div class="progress-bar progress-bar-striped"  role="progressbar"
>            aria-valuenow="${amountCompleted}" aria-valuemin="0"
>            aria-valuemax="${amountTotal}" style="width: ${percentCompleted}%">
>         <percentCompleted/>%
>       </div>
>     </div>
>   </ifFinished>
> </myJobStatus>

This will poll for updates every 300 milliseconds.  See the documentation for
'statusSplice' for more details.

To get the above code working, you would have the myJobStatusPage handler
return markup that contains something like this:

> <h1>Status</h1>
> <div class="statusdiv col-md-4">
>   <apply template="statusTemplate"/>
> </div>

You will also need to bind the main splice provided by this module.

> splices = do
>     ...
>     "myJobStatus" MS.## statusSplice splices getUrl getMyJobStatus isFinished

You need to bind this splice once for each type of action that you are
polling, each with its own splice name and function for getting the job
status.

-}

module Snap.Extras.PollStatus
  ( jobStatusHandler
  , statusSplice

  -- * Generic Status Type

  , JobState(..)
  , jobFinished
  , statusFinished
  , Status(..)
  , statusPercentCompleted
  , statusElapsed
  , statusSplices
  ) where

------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder.Char8 as BB
import           Control.Applicative            as A
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.ByteString                (ByteString)
import qualified Data.Map.Syntax                as MS
import           Data.Maybe
import           Data.Monoid                    (mempty)
import           Data.Readable
import           Data.Text
import qualified Data.Text                      as T
import           Data.Time
import           Heist
import           Heist.Compiled
import           Heist.Splices
import           Language.Javascript.JMacro
import           Snap.Snaplet
import           Snap.Snaplet.Heist.Compiled    (HasHeist)
import           Text.PrettyPrint.Leijen.Text   (renderOneLine)
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------
import           Snap.Extras.Ajax
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Top-level handler that handles the job polling infrastructure.
jobStatusHandler
    :: HasHeist b
    => ByteString
    -- ^ Name of the template to use for the updateStatus endpoint.
    -> T.Text
    -- ^ A CSS selector string used to substitute the contents of the AJAX
    -- template into the status page.
    -> Handler b v ()
jobStatusHandler updateTemplate selector =
    replaceWithTemplate updateTemplate selector


------------------------------------------------------------------------------
-- | Function to generate a status splice.  This splice makes the following
-- splices available to its children:
--
-- Conditional splices:
-- ifPending, ifRunning, ifNotFinished, ifFinished, ifFinishedSuccess,
-- ifFinishedFailure
--
-- Data splices:
-- elapsedSeconds, startTime, endTime, jobState, messages, percentCompleted,
-- amountCompleted, amountTotal
statusSplice
    :: Monad n
    => Splices (RuntimeSplice n status -> Splice n)
    -- ^ Splices defined for your job status type status.
    -> n (Maybe Text)
    -- ^ Handler to get the URL that should be used for requesting AJAX status
    -- updates.  This must be a handler because it will usually contain some
    -- form of job ID that is only obtainable at runtime.
    -> n (Maybe status)
    -- ^ Handler that knows how to get the status of the job in question.
    -- This typically will require it to know how to get some sort of job ID
    -- from the request.
    -> (status -> Bool)
    -- ^ A function to tell whether the status is finished
    -> Splice n
statusSplice splices getUrl getStatus isFinished = do
    n <- getParamNode
    runAttrs <- runAttributesRaw $ X.elementAttrs n
    let nodes = X.elementChildren n ++ [X.Element "updateJs" [] []]
        run = runNodeList nodes
    mayDeferMap return (withSplices run allSplices) $ do
        attrs <- runAttrs
        let delay = fromMaybe 1000 $ fromText =<< lookup "interval" attrs
        runMaybeT $ do
            url <- MaybeT $ lift getUrl
            let js = T.concat ["<script>", updateJS url delay, "</script>"]
            s <- MaybeT $ lift getStatus
            return (js, s)
  where
    allSplices = do
        "updateJs" MS.## pureSplice internalUpdate
        MS.mapV (. liftM snd) splices
    internalUpdate (js,s) =
        if not $ isFinished s
          then BB.fromText js
          else mempty


------------------------------------------------------------------------------
-- | JS code to do an AJAX request to updateStatus after a delay.
updateJS
    :: Text
    -- ^ The url for incremental status updates
    -> Int
    -- ^ The amount of time to wait before requesting the page in milliseconds
    -> Text
updateJS url delay = T.pack $ show $ renderOneLine $ renderJs $
    [jmacro|
      setTimeout(function() {
        $.get(`(T.unpack url)`);
      }, `(show delay)`);
    |]


------------------------------------------------------------------------------
--                          Example Implementation
------------------------------------------------------------------------------


-- The above functions are everything you need to use the polling
-- infrastructure using yo

------------------------------------------------------------------------------
-- | Enumeration of the states a job can be in.
data JobState = Pending | Running | FinishedSuccess | FinishedFailure
  deriving (Eq,Show,Read,Enum)


------------------------------------------------------------------------------
-- | Returns a bool indicating whether the job is finished.
jobFinished :: JobState -> Bool
jobFinished FinishedSuccess = True
jobFinished FinishedFailure = True
jobFinished _               = False


------------------------------------------------------------------------------
-- | Returns a bool indicating whether the job is finished.
statusFinished :: Status -> Bool
statusFinished = jobFinished . statusJobState


------------------------------------------------------------------------------
-- | The complete status of a job.
data Status = Status
    { statusStartTime       :: Maybe UTCTime
    , statusTimestamp       :: UTCTime
    , statusJobState        :: JobState
    , statusMessages        :: [Text]
    , statusAmountCompleted :: Double
    , statusAmountTotal     :: Double
    } deriving (Eq,Show)


------------------------------------------------------------------------------
-- | Calculates the percent completed as an Int.
statusPercentCompleted :: Status -> Int
statusPercentCompleted Status{..} =
    round $ 100.0 * statusAmountCompleted / statusAmountTotal


------------------------------------------------------------------------------
tshow :: Show a => a -> Text
tshow = T.pack . show


------------------------------------------------------------------------------
-- | The status splices
statusSplices
    :: Monad n
    => Splices (RuntimeSplice n Status -> Splice n)
statusSplices = do
    "ifPending" MS.## ifCSplice ((==Pending) . statusJobState)
    "ifRunning" MS.## ifCSplice ((==Running) . statusJobState)
    "ifNotFinished" MS.## ifCSplice (not . statusFinished)
    "ifFinished" MS.## ifCSplice (statusFinished)
    "ifFinishedSuccess" MS.## ifCSplice ((==FinishedSuccess) . statusJobState)
    "ifFinishedFailure" MS.## ifCSplice ((==FinishedFailure) . statusJobState)
    "messages" MS.## manyWithSplices runChildren
                    ("msgText" MS.## pureSplice (textSplice id)) .
                    (liftM $ statusMessages)
    MS.mapV pureSplice $ do
      "startTime" MS.## textSplice (tshow . statusStartTime)
      "endTime" MS.## textSplice (tshow . statusTimestamp)
      "jobState" MS.## textSplice (tshow . statusJobState )
      "percentCompleted" MS.## textSplice (tshow . statusPercentCompleted)
      "amountCompleted" MS.## textSplice (tshow . statusAmountCompleted)
      "amountTotal" MS.## textSplice (tshow . statusAmountTotal)


------------------------------------------------------------------------------
statusElapsed :: Status -> Maybe Int
statusElapsed Status{..} =
    round . diffUTCTime statusTimestamp A.<$> statusStartTime


