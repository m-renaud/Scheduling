{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Scheduling.JobUtils.Tests
       ( htf_thisModulesTests
       ) where

import Test.Framework

import Scheduling.JobUtils
import Scheduling.Types

-- Super simple tests just to try out HUnit.
test_isFinished_Finished = assertEqual True (isFinished Finished)
test_isFinished_Unfinished = assertEqual False (isFinished Unfinished)
