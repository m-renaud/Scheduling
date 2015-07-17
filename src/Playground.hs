module Playground where

import Data.HashPSQ as PSQ

import Driver
import Scheduling.Algorithm.RoundRobin as RR
import Scheduling.Algorithm.ShortestJobFirst as SJF
import Scheduling.Driver.CustomDrivers
import Scheduling.Types

-- | Sample list of jobs to schedule.
jobList :: [Job]
jobList = [mkJob "A" 4 (Priority 3),
           mkJob "B" 2 (Priority 8),
           mkJob "C" 7 (Priority 0)]

-- | Run the round robin scheduling algorithm on 'jobList' with 2 cpu second time slices.
--   The scheduler's decision will be printed at each step and at the end the list of
--   jobs in the order they completed will be returned.
roundRobinSample :: IO ((), [String])
roundRobinSample = runJobs jobList RR.preprocessJobs (RR.scheduleJob 2)

sjfSample :: IO ((), [String])
sjfSample = runJobs jobList SJF.preprocessJobs SJF.scheduleJob
