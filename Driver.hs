module Driver
       ( runJobsDriver
       , SchedulerDriverActions(..)
       ) where

import Control.Monad (when)
import Control.Monad.Trans.Cont

import Scheduling.JobUtils
import Scheduling.Types

-- | The actions to perform on each of the stages of the scheduling algorithm driver.
data SchedulerDriverActions t j = ClockCycles Int
                                | BetweenCycles
                                | OnDecision (SchedulerDecision t j)
                                | JobFinished Job
                                | Done

-- | The driver for a scheduling algorithm.
--   Schedule the list of jobs given a scheduling algorithm, the number of cycles used so
--   far, and actions for each of the continuations.
runJobsDriver :: Monad m => t j -> Scheduler t j -> Int -> ContT () m (SchedulerDriverActions t j)
runJobsDriver jobs runScheduler cycles = ContT $ \performAction -> do
  performAction (ClockCycles cycles)
  performAction BetweenCycles
  case runScheduler jobs of
   Nothing -> return ()
   Just decision@(SchedulerDecision job status cyclesUsed remainingJobs) -> do
     performAction $ OnDecision decision
     when (isFinished status) $ performAction (JobFinished job)
     runContT (runJobsDriver remainingJobs runScheduler (cycles + cyclesUsed)) performAction
