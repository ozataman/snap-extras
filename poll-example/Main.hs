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

newJob :: UTCTime -> JobRepo -> (JobRepo, Int)
newJob ts repo = (JobRepo (i+1) jobs, i)
  where
    i = repoNextInd repo
    jobs = M.insert i (Status ts Running Nothing [] 0 100) $ repoJobs repo

updateJob :: Int -> (Status -> Status) -> JobRepo -> (JobRepo, ())
updateJob jobId f repo = (JobRepo (repoNextInd repo) newJobs, ())
  where
    curJobs = repoJobs repo
    newJobs = M.update (g . f) jobId curJobs
    g s = if srAmountCompleted s == srAmountTotal s
            then Just $ s { srJobState = FinishedSuccess }
            else Just s

jobAction
    :: IORef JobRepo
    -> Int
    -> UTCTime
    -> Double
    -> IO ()
jobAction ref jobId ts seconds = do
    let inc = seconds / 100.0
        numIters = ceiling $ seconds / inc
    forM_ [1..numIters] $ \n -> do
        threadDelay $ round $ inc * 1000000
        let setStatus _ = Status ts Running Nothing [] (fromIntegral n * inc) seconds
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

routes = [ ("hello", writeText "hello world")
         , ("myjobs", myJobsHandler)
         , ("", heistServe)
         , ("", serveDirectory "static")
         ]

appInit = makeSnaplet "app" "blah" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit' "" $ mempty
        { hcLoadTimeSplices = defaultLoadTimeSplices
        , hcCompiledSplices = splices
        }
    addRoutes routes
    ref <- liftIO $ newIORef emptyJobRepo
    return $ App h ref

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
    "jobId" ## jobIdSplice
    "jobUpdateJs" ## updateSplice
    -- You need one of these status splices per status job type
    "jobStatus" ## statusSplice getMyJobStatus


------------------------------------------------------------------------------
-- | Starts a job and returns its job ID.
startlongjob :: Handler App App T.Text
startlongjob = do
    ref <- gets _repo
    ts <- liftIO getCurrentTime
    jobId <- liftIO $ atomicModifyIORef' ref (newJob ts)
    liftIO $ forkIO $ jobAction ref jobId ts 10
    return $ T.pack $ show jobId


------------------------------------------------------------------------------
-- | This is the main handler that you add to your app routes.
myJobsHandler :: Handler App App ()
myJobsHandler = do
    jobsHandler startlongjob (render "jobstatus") "_status" ".statusdiv"


------------------------------------------------------------------------------
-- | A function that gets the status of a job.
getMyJobStatus :: T.Text -> Handler App App (Maybe Status)
getMyJobStatus jid = do
    ref <- gets _repo
    repo <- liftIO $ readIORef ref
    return $ do
        jobId <- fromText jid
        M.lookup jobId (repoJobs repo)


