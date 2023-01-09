module Core where 

import qualified DockerLocal as Docker
import qualified RIO.Map as Map
import RIO
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text
import Data.List as List
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Aeson as Aeson 
import qualified Codec.Serialise as Serialise



data Pipeline 
  = Pipeline 
   { steps :: NonEmpty Step 
   } 
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)
  
  
data Step
 = Step  
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Docker.Image
    }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)


-- Add 'Ord' to make it Map Key
newtype StepName = StepName Text
 deriving (Eq,Ord,Show, Generic, Aeson.FromJSON, Serialise.Serialise)
 
 
newtype BuildNumber = BuildNumber Int
  deriving (Eq,Show, Generic, Serialise.Serialise, Ord)
  
buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

  
data Build
 = Build
    { pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
    , volume :: Docker.Volume
    }
  deriving (Eq,Show, Generic, Serialise.Serialise)

data StepResult
 = StepFailed Docker.ContainerExitCode
 | StepSucceeded
 deriving (Eq,Show, Generic, Serialise.Serialise)
 
exitCodeToInt :: Docker.ContainerExitCode -> Int
exitCodeToInt (Docker.ContainerExitCode code) = code

data BuildState 
 = BuildReady
 | BuildRunning BuildRunningState
 | BuildFinished BuildResult
 deriving (Eq,Show, Generic, Serialise.Serialise)
 
data BuildRunningState
 = BuildRunningState
    { step :: StepName
    , container :: Docker.ContainerID
    }
 deriving (Eq,Show, Generic, Serialise.Serialise)
 
data BuildResult
 = BuildSucceeded
 | BuildFailed
 | BuildUnexpectedState Text
 deriving (Eq,Show, Generic, Serialise.Serialise)
  
stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build = 
 if allSucceeded
    then case nextStep of 
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
 where 
   allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
   nextStep = List.find f build.pipeline.steps
   f step = not $ Map.member step.name build.completedSteps



progress :: Docker.Service -> Build -> IO Build
progress docker build = 
 case build.state of
  BuildFinished _ -> pure build
  
  BuildReady -> 
    case buildHasNextStep build of 
      Left result -> 
        pure $ build{ state = BuildFinished result }
      Right step -> do
        let script = Text.unlines $ ["set -ex"] <> NonEmpty.toList step.commands
            options = Docker.CreateContainerOptions 
                        { image = step.image
                        , script = script
                        , volume = build.volume
                        }
        docker.pullImage step.image
        container <- docker.createContainer options
        docker.startContainer container

        let s = BuildRunningState 
                    { step = step.name
                    , container = container 
                    }
        pure $ build{ state = BuildRunning s }
        
  BuildRunning state -> do
    status <- docker.containerStatus state.container
    
    case status of 
      Docker.ContainerRunning ->
        pure build
      Docker.ContainerExited exit -> do
        -- move step to completed steps            
        let result = exitCodeToStepResult exit
            
        pure build
          { state = BuildReady
          , completedSteps = Map.insert state.step result build.completedSteps
          }
      
      Docker.ContainerOther other -> do
        -- handle error
        --traceShowIO "DockerContainerOther"
        let s = BuildUnexpectedState other
        pure $ build{state = BuildFinished s}



exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit = 
 if exitCodeToInt exit == 0 
    then StepSucceeded
    else StepFailed exit
    
    
type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerID Time.POSIXTime
  | CollectionFinished
  deriving (Eq,Show)
  
data Log = Log
  { output :: ByteString
  , step :: StepName
  }
  deriving (Eq,Show, Generic, Serialise.Serialise)
  
collectLogs
  :: Docker.Service
  -> LogCollection
  -> Build
  -> IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateCollection build.state now collection
  pure (newCollection, logs)
  
initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $ NonEmpty.toList steps
  where steps = pipeline.steps <&> \step -> (step.name, CollectionReady)
  
updateCollection
  :: BuildState
  -> Time.POSIXTime
  -> LogCollection
  -> LogCollection
updateCollection state lastCollection collection = 
  Map.mapWithKey f collection
  where
    update step since nextState =
        case state of 
          BuildRunning state ->
            if state.step == step
              then CollectingLogs state.container 0
              else CollectionReady
          _ -> CollectionReady
          
    f step = \case
      CollectionReady ->
        update step 0 CollectionReady
      
      CollectingLogs _ _ -> 
       update step lastCollection CollectionFinished
      
      CollectionFinished -> CollectionFinished


displayBuildNumber :: BuildNumber -> String
displayBuildNumber number = "#" <> show (buildNumberToInt number)


runCollection
  :: Docker.Service
  -> Time.POSIXTime
  -> LogCollection
  -> IO [Log]
runCollection docker collectUntil collection = do
  logs <- Map.traverseWithKey f collection
  pure $ concat (Map.elems logs)
  where 
    f step = \case 
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options = 
              Docker.FetchLogsOptions
                { container = container
                , since = since 
                , until = collectUntil
                }
        output <- docker.fetchLogs options
        pure [Log {step = step, output = output}]