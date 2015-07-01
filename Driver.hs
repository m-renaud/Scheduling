module Driver where

import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Writer.Class
import Data.Maybe (fromJust)
  
import Scheduling.Types

-- | The actions to perform on each of the stages of the scheduling algorithm driver.
data SchedulerDriverActions t = ClockCycles Int
                              | BetweenCycles
                              | OnDecision (SchedulerDecision t)
                              | JobFinished Job
                              | Done

-- | Run the provided 'jobs':
--   - Print the number of cycles used so far each iteration.
--   - Perform no action between cycles.
--   - When a decision is made, print it.
--   - When a job finished, add the job name to a list.
--   - When all processes have completed, do nothing else.
runJobs :: t Job -> (t Job -> s Job) -> Scheduler s -> IO ((), [String])
runJobs jobs preprocessor scheduler = 
  runWriterT $ runContT (runJobsDriver (preprocessor jobs) scheduler 0) action
  where action :: SchedulerDriverActions t -> WriterT [String] IO ()
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
runJobsDelay jobs preprocessor scheduler =
  runContT (runJobsDriver (preprocessor jobs) scheduler 0) action
  where action (ClockCycles cycles)  = printCycles cycles
        action  BetweenCycles        = threadDelay 1000000
        action (OnDecision decision) = printDecision decision
        action (JobFinished job)     = printJob job
        action  Done                 = return ()

-- | The driver for a scheduling algorithm.
--   Schedule the list of jobs given a scheduling algorithm, the number of cycles used so
--   far, and actions for each of the continuations.
runJobsDriver :: Monad m => t Job -> Scheduler t -> Int -> ContT () m (SchedulerDriverActions t)
runJobsDriver jobs runScheduler cycles = ContT $ \performAction -> do
  performAction (ClockCycles cycles)
  performAction BetweenCycles
  case runScheduler jobs of
   Nothing -> return ()
   Just decision@(SchedulerDecision job status cyclesUsed remainingJobs) -> do
     performAction $ OnDecision decision
     when (isFinished status) $ performAction (JobFinished job)
     runContT (runJobsDriver remainingJobs runScheduler (cycles + cyclesUsed)) performAction


-- Simple helper functions
printCycles :: Int -> IO ()
printCycles cycles = putStrLn $ "Cycles: " ++ show cycles

printDecision :: SchedulerDecision t -> IO ()
printDecision (SchedulerDecision job status cycles remainingJobs) =
  putStrLn $ "Running job " ++ jobName job ++ " for " ++ show cycles ++ " cycles."

printJob :: Job -> IO ()
printJob job = putStrLn $ "Finished job " ++ jobName job

   
