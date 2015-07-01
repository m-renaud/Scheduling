module Scheduling.Algorithm.RoundRobin
       ( preprocessJobs
       , addNewJobs
       , scheduleJob
       ) where

import DataStructures.Queue
  
import Scheduling.JobUtils
import Scheduling.Types

preprocessJobs :: [Job] -> Queue Job
preprocessJobs = fromList

addNewJobs :: Queue Job -> [Job] -> Queue Job
addNewJobs existingJobs = foldl enq existingJobs

scheduleJob :: CpuTime -> Scheduler Queue
scheduleJob time jobs
  | empty jobs = Nothing
  | otherwise  = Just $ SchedulerDecision job outcome (min time timeRemaining) remainingJobs
  where (job, jobs') = deq jobs
        timeRemaining = jobTimeRemaining job
        (outcome, remainingJobs) =
          if timeRemaining <= time
          then (Finished, jobs')
          else (Unfinished, enq jobs' (updateTimeRemaining job time))
                         