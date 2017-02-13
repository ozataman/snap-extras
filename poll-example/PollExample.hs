{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Aeson
import           Data.IORef
import qualified Data.Map                    as M
import qualified Data.Map.Syntax             as MS
import           Data.Monoid
import           Data.Readable
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time
import           Heist
import           Heist.Compiled
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Heist.Compiled
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Snap.Extras.CoreUtils
import           Snap.Extras.PollStatus
------------------------------------------------------------------------------

------------------------------------------------------------------------------
--         Job infrastructure that would have been necessary anyway
------------------------------------------------------------------------------


data JobRepo = JobRepo
    { repoNextInd :: Int
    , repoJobs    :: M.Map Int Status
    }

emptyJobRepo = JobRepo 0 M.empty

newJob :: UTCTime -> UTCTime -> JobRepo -> (JobRepo, Int)
newJob ts curTs repo = (JobRepo (i+1) jobs, i)
  where
    i = repoNextInd repo
    jobs = M.insert i (Status (Just ts) curTs Running [] 0 10) $
             repoJobs repo

updateJob :: Int -> (Status -> Status) -> JobRepo -> (JobRepo, ())
updateJob jobId f repo = (JobRepo (repoNextInd repo) newJobs, ())
  where
    curJobs = repoJobs repo
    newJobs = M.update (g . f) jobId curJobs
    g s = if statusAmountCompleted s == statusAmountTotal s
            then Just $ s { statusJobState = FinishedSuccess }
            else Just s

jobAction
    :: IORef JobRepo
    -> Int
    -> Double
    -> IO ()
jobAction ref jobId seconds = do
    let inc = seconds / 100.0
        numIters = ceiling $ seconds / inc
    forM_ [1..numIters] $ \n -> do
        threadDelay $ round $ inc * 1000000
        ts <- liftIO getCurrentTime
        let setStatus s = s { statusTimestamp = ts
                            , statusAmountCompleted = (fromIntegral n * inc) }
        atomicModifyIORef' ref (updateJob jobId setStatus)


------------------------------------------------------------------------------
--                    Web app boilerplate for this test
------------------------------------------------------------------------------


data App = App
    { _heist :: Snaplet (Heist App)
    , _repo  :: IORef JobRepo
    }
makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

routes = [ ("startJob", startlongjob)
         , ("myJobStatus", jobStatusHandler "_status" ".statusdiv")
         , ("", heistServe)
         , ("", serveDirectory "static")
         ]

appInit = makeSnaplet "app" "blah" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "" hc
    addRoutes routes
    ref <- liftIO $ newIORef emptyJobRepo
    return $ App h ref
  where
    hc = emptyHeistConfig
      & hcLoadTimeSplices .~ defaultLoadTimeSplices
      & hcCompiledSplices .~ splices

main :: IO ()
main = do
    serveSnaplet defaultConfig appInit


------------------------------------------------------------------------------
--                 Core code needed to create a polling job
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | You have to define at least these three splices in your application.  The
-- first two are static.  You'll need an instance of the third one for each
-- type of job you need status for.
splices = do
    -- You need one of these status splices per status job type
    "jobStatus" MS.## statusSplice statusSplices getUrl getMyJobStatus
                                   statusFinished
  where
    getUrl = do
        jobId <- getParam "jobId"
        return $ fmap (T.decodeUtf8 . ("myJobStatus?jobId=" <>)) jobId


------------------------------------------------------------------------------
-- | Starts a job and returns its job ID.
startlongjob :: Handler App App ()
startlongjob = do
    ref <- gets _repo
    ts <- liftIO getCurrentTime
    jobId <- liftIO $ atomicModifyIORef' ref (newJob ts ts)
    liftIO $ forkIO $ jobAction ref jobId 10
    redirect $ T.encodeUtf8 $ "jobstatus?jobId=" <> (T.pack $ show jobId)


------------------------------------------------------------------------------
-- | A function that gets the status of a job.
getMyJobStatus :: Handler App App (Maybe Status)
getMyJobStatus = do
    ref <- gets _repo
    repo <- liftIO $ readIORef ref
    mjidbs <- getParam "jobId"
    return $ do
        jobId <- fromBS =<< mjidbs
        M.lookup jobId (repoJobs repo)
