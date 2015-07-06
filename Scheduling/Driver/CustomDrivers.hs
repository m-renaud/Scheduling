module Scheduling.Driver.CustomDrivers
       ( runJobs
       , runJobsInteractive
       , runJobsWithDelay
       ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Trans.Cont
import Control.Monad.Writer.Class

import Driver
import Scheduling.Types

-- | Run the provided 'jobs':
--   - Print the number of cycles used so far each iteration.
--   - Perform no action between cycles.
--   - When a decision is made, print it.
--   - When a job finished, add the job name to a list.
--   - When all processes have completed, do nothing else.
runJobs :: t Job -> (t Job -> s j) -> Scheduler s j -> IO ((), [String])
runJobs jobs preprocessor scheduler =
  runWriterT $ runContT (runJobsDriver (preprocessor jobs) scheduler 0) action
  where action :: SchedulerDriverActions t j -> WriterT [String] IO ()
        action (ClockCycles cycles)  = liftIO $ printCycles cycles
        action  BetweenCycles        = return ()
        action (OnDecision decision) = liftIO $ printDecision decision
        action (JobFinished job)     = tell [jobName job]
        action  Done                 = return ()

-- | Run jobs interactively, woiting for a keypress between decisions.
runJobsInteractive jobs preprocessor scheduler =
  runContT (runJobsDriver (preprocessor jobs) scheduler 0) action
  where action (ClockCycles cycles)  = printCycles cycles
        action  BetweenCycles        = void getChar
        action (OnDecision decision) = printDecision decision
        action (JobFinished job)     = printJob job
        action  Done                 = return ()

-- | Run jobs with a 1 sec delay between decisions.
runJobsWithDelay jobs preprocessor scheduler =
  runContT (runJobsDriver (preprocessor jobs) scheduler 0) action
  where action (ClockCycles cycles)  = printCycles cycles
        action  BetweenCycles        = threadDelay 1000000
        action (OnDecision decision) = printDecision decision
        action (JobFinished job)     = printJob job
        action  Done                 = return ()


-- Simple helper functions
printCycles :: Int -> IO ()
printCycles cycles = putStrLn $ "Cycles: " ++ show cycles

printDecision :: SchedulerDecision t j -> IO ()
printDecision (SchedulerDecision job status cycles remainingJobs) =
  putStrLn $ "Running job " ++ jobName job ++ " for " ++ show cycles ++ " cycles."

printJob :: Job -> IO ()
printJob job = putStrLn $ "Finished job " ++ jobName job
