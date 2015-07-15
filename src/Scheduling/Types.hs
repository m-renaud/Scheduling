-- | module: Scheduling.Types
--
-- Types to be used by scheduling algorithms.

module Scheduling.Types
       ( Job(..)
       , CpuTime
       , Priority(..)
       , JobStatus(..)
       , Scheduler
       , SchedulerDecision(..)
       , mkJob
       ) where

data Job =
  Job { jobName          :: String
      , jobTotalTime     :: CpuTime
      , jobTimeRemaining :: CpuTime
      , jobPriority      :: Priority
      } deriving (Eq)

instance Show Job where
  show (Job name totalTime timeRemaining priority) =
    "Job {" ++ show name ++ " P=" ++ show priority
    ++ " (" ++ show timeRemaining ++ "/" ++ show totalTime ++ ")}"

mkJob :: String -> CpuTime -> Priority -> Job
mkJob name time p = Job { jobName          = name
                        , jobTotalTime     = time
                        , jobTimeRemaining = time
                        , jobPriority      = p
                        }

type CpuTime = Int

data Priority = Priority Int
              deriving (Eq, Ord, Read, Show)


-- | Indicates whether the most previously run job by the scheduling algorithm
-- | finished.
data JobStatus = Finished | Unfinished
               deriving (Eq, Read, Show)

-- | The decision made by the scheduler.
data SchedulerDecision t j =
  SchedulerDecision { decisionJobToRun      :: Job  -- ^ The job to be run next.
                    , decisionJobStatus     :: JobStatus  -- ^ The status of the job after running.
                    , decisionCpuTimeUsed   :: CpuTime  -- ^ The CPU time to allocate to the job.
                    , decisionJobsRemaining :: t j  -- ^ The jobs that still need to be scheduled.
                    } 

-- | A scheduling algorithm where t is the internal representation used to store the jobs.
type Scheduler t j =
  -- | A sequence of jobs to schedule, already in the internal representation
  t j
  -- | Just the decision made by the scheduler, Nothing if no jobs to schedule.
  -> Maybe (SchedulerDecision t j) 

