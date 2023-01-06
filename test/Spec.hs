module Main where

import RIO
import RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Set as Set
import qualified RIO.ByteString as ByteString
import qualified RIO.Map as Map
import Core
import qualified Runner 
import qualified DockerLocal as Docker
import qualified JobHandler
import qualified JobHandler.Memory
import qualified Server
import qualified Agent
import Test.Hspec
import System.Process as Process
import Control.Monad (when)
import qualified Data.Yaml as Yaml
import qualified Control.Concurrent.Async as Async




main :: IO ()
main = hspec do
 docker <- runIO Docker.createService
 runner <- runIO $ Runner.createService docker
 
 beforeAll cleanupDocker $ describe "Quad CI" do
  -- it "should run a build (success)" do
    -- testRunSuccess runner
  -- it "should run a build (failure)" do
    -- testRunFailure runner    
  -- it "should share workspace between steps" do
    -- testSharedWorkspace docker runner
  -- it "should collect logs" do
    -- testLogCollection runner
  -- it "should pull images" do
    -- testImagePull runner
  it "should decode pipelines" do
    testYamlDecoding runner
  it "should run server and agent" do
    testServerAndAgent runner

 
-- test one
testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
 build <- runner.prepareBuild $ makePipeline
                [ makeStep "First step" "ubuntu" ["date"]
                , makeStep "Second step" "ubuntu" ["uname -r"]
                ]
 result <- runner.runBuild emptyHooks build
 result.state `shouldBe` BuildFinished BuildSucceeded
 Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
 
-- test two
testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
    build <- runner.prepareBuild $ makePipeline
                [ makeStep "Should fail" "ubuntu" ["exit 1"]
                ]
    result <- runner.runBuild emptyHooks build
    
    result.state `shouldBe` BuildFinished BuildFailed
    Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]
    
    
-- test three
testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
  build <- runner.prepareBuild $ makePipeline
            [ makeStep "Create file" "ubuntu" ["echo hello > test", "ls"] -- "cat -n test",
            , makeStep "Read file" "ubuntu" ["cat test"] -- "cd /app","ls",
            ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]


-- test four
testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do 
  expected <- newMVar $ Set.fromList ["hello","world","Linux"]
  
  let onLog :: Log -> IO ()
      onLog log = do
        remaning <- readMVar expected
        --traceShowIO remaning
        forM_ remaning $ \word -> do
          case ByteString.breakSubstring word log.output of
            (_,"") -> do
              --traceShowIO word
              --traceShowIO log.output
              --traceShowIO $ ByteString.breakSubstring word log.output
              pure () -- Not found
            _ -> modifyMVar_ expected (pure . Set.delete word) 
            
  let hooks = Runner.Hooks { logCollected = onLog }
  
  build <- runner.prepareBuild $ makePipeline
            [ makeStep "Long step" "ubuntu" ["echo hello","sleep 2","echo world"]
            , makeStep "Echo Linux" "ubuntu" ["uname -s"]
            ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  readMVar expected >>= \logs -> logs `shouldBe` Set.empty


--test five
testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.callCommand "docker rmi -f busybox"
  
  build <- runner.prepareBuild $ makePipeline
            [ makeStep "First step" "busybox" ["date"]
            ]
  result <- runner.runBuild emptyHooks build
  
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]
  
  
--test 6
testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "C:\\Users\\madOr\\Documents\\pipeline-sample.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  
  
--test 7
testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent runner = do
  handler <- JobHandler.Memory.createService
  
  serverThread <- Async.async do
    Server.run (Server.Config 9000) handler
  
  Async.link serverThread
  
  agentThread <- Async.async do
    Agent.run (Agent.Config "http://localhost:9000") runner
  
  Async.link agentThread

  let pipeline = makePipeline
        [ makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]
        ]
  number <- handler.queueJob pipeline
  checkBuild handler number
  
  Async.cancel serverThread
  Async.cancel agentThread
  
  

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
  where 
    loop = do
      Just job <- handler.findJob number
      case job.state of
        JobHandler.JobScheduled build -> do
          case build.state of
            BuildFinished s -> s `shouldBe` BuildSucceeded
            _ -> loop
        _ -> loop



cleanupDocker :: IO ()
cleanupDocker = void do
  containersList <- liftM lines $ Process.readCreateProcess ( Process.shell "docker ps -f label=\"quad\" -aq" ) ""
  volumesList <- liftM lines $ Process.readCreateProcess ( Process.shell "docker volume ls -f label=\"quad\" -q" ) ""
  let rmCntrs = "docker rm " ++ containers
      rmVolumes = "docker volume rm " ++ volumes
      containers = concat $ map (\x -> show x ++ " ") containersList
      volumes = concat $ map (\x -> show x ++ " ") volumesList
  when (containers /= []) $ Process.callCommand rmCntrs --Process.createProcess ( Process.shell rmCntrs) ""
  when (volumes /= []) $ Process.callCommand rmVolumes--Process.createProcess ( Process.shell rmVolumes) ""
 --traceShowIO ans
 --cids <- Docker.listQUADcontainers
 --Docker.removeContainers cids
  pure ()
 
    


emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = \_ -> pure ()
    } 
    
    
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
 = Step 
    { name = StepName name
    , image = Docker.Image { name = image, tag = "latest" }
    , commands = NonEmpty.Partial.fromList commands
    }
    
    
makePipeline :: [Step] -> Pipeline
makePipeline steps
 = Pipeline { steps = NonEmpty.Partial.fromList steps }
    
    
runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
 newBuild <- Core.progress docker build
 case newBuild.state of
  BuildFinished _ ->
   pure newBuild
  _ -> do
   threadDelay (1*1000*1000)
   Main.runBuild docker newBuild