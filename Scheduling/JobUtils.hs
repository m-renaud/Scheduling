{-# LANGUAGE RecordWildCards #-}

module Scheduling.JobUtils
       ( updateTimeRemaining
       , priorityOrder
       , timeRemainingOrder
       ) where

import Data.Ord (comparing)

import Scheduling.Types

updateTimeRemaining :: Job -> CpuTime -> Job
updateTimeRemaining j@Job{..} t = j { jobTimeRemaining = jobTimeRemaining - t }

priorityOrder :: Job -> Job -> Ordering
priorityOrder = comparing jobPriority

timeRemainingOrder :: Job -> Job -> Ordering
timeRemainingOrder = comparing jobTimeRemaining
