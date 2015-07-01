module Playground where

import Driver
import Scheduling.Algorithm.RoundRobin as RR
import Scheduling.Types

-- | Sample list of jobs to schedule.
jobList :: [Job]
jobList = [mkJob "A" 4 (Priority 0), mkJob "B" 6 (Priority 0), mkJob "C" 7 (Priority 0)]

