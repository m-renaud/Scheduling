module Scheduling.Algorithm.ShortestJobFirst
       ( preprocessJobs
       , scheduleJob
       ) where

import Data.HashPSQ

import Scheduling.JobUtils
import Scheduling.Types

newtype SjfJob = SjfJob Job deriving (Eq, Show)

instance Ord SjfJob where
  compare (SjfJob l) (SjfJob r) = timeRemainingOrder l r

preprocessJobs :: [Job] -> HashPSQ String CpuTime SjfJob
preprocessJobs = Data.HashPSQ.fromList . fmap (\j -> (jobName j, jobTotalTime j, SjfJob j))

scheduleJob :: Scheduler (HashPSQ String CpuTime) SjfJob
scheduleJob jobs = case findMin jobs of
  Nothing -> Nothing
  Just (name, time, SjfJob job) -> Just $ SchedulerDecision job Finished time (deleteMin jobs)

