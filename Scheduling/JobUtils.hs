{-# LANGUAGE RecordWildCards #-}

module Scheduling.JobUtils
       ( isFinished
       , updateTimeRemaining
       , priorityOrder
       , timeRemainingOrder
       ) where

import Data.Ord (comparing)

import Scheduling.Types

isFinished :: JobStatus -> Bool
isFinished Finished = True
isFinished _        = False

updateTimeRemaining :: Job -> CpuTime -> Job
updateTimeRemaining j@Job{..} t = j { jobTimeRemaining = jobTimeRemaining - t }

priorityOrder :: Job -> Job -> Ordering
priorityOrder = comparing jobPriority

timeRemainingOrder :: Job -> Job -> Ordering
timeRemainingOrder = comparing jobTimeRemaining
