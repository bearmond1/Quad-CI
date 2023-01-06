module DockerLocal where

import RIO
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Time.Clock.POSIX as Time
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text.Partial
import qualified Codec.Serialise as Serialise



data Service
 = Service
    { createContainer :: CreateContainerOptions -> IO ContainerID
    , startContainer :: ContainerID -> IO ()
    , containerStatus :: ContainerID -> IO ContainerStatus
    , createVolume :: IO Volume
    , fetchLogs :: FetchLogsOptions -> IO ByteString
    , pullImage :: Image -> IO ()
    }
    
    
createService :: IO Service
createService = do
 pure Service
  { createContainer = createContainer_
  , startContainer = startContainer_
  , containerStatus = containerStatus_
  , createVolume = createVolume_
  , fetchLogs = fetchLogs_
  , pullImage = pullImage_
  }


  
data Image = Image { name :: Text
                   , tag :: Text  }
  deriving (Eq,Show, Generic, Serialise.Serialise)
  
instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "parse-image" $ \image -> do
    case Text.Partial.splitOn ":" image of
      [name] -> pure $ Image { name = name, tag = "latest" }
      [name,tag] -> pure $ Image { name = name, tag = tag }
      _ -> fail $ "Image has too many colons " <> Text.unpack image


pullImage_ :: Image -> IO ()
pullImage_ image = do
  let path = "v1.41/images/create?tag=" 
          <> image.tag
          <> "&fromImage="
          <> image.name
      req = HTTP.defaultRequest
          & HTTP.setRequestPort 2375
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req
  
createVolume_ :: IO Volume
createVolume_ = do
  let body = Aeson.object
                [ ("Labels", Aeson.object [("quad","")])
                ]
      req = HTTP.defaultRequest
          & HTTP.setRequestPath "v1.41/volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
          & HTTP.setRequestPort 2375
      parser = Aeson.withObject "create-volume" $ \o -> do
        name <- o .: "Name"
        pure $ Volume name
  res <- HTTP.httpBS req
  --traceShowIO $ HTTP.getResponseBody res
  parseResponse res parser
  
  
fetchLogs_ :: FetchLogsOptions -> IO ByteString
fetchLogs_ options = do
  let timestampToText t = tshow ( round t :: Int )
      path = "v1.41/containers/" 
          <> containerIDtoText options.container
          <> "/logs?stdout=true&stderr=true&since="
          <> timestampToText options.since
          <> "&until="
          <> timestampToText options.until
      req = HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestMethod "GET"
          & HTTP.setRequestPort 2375
  res <- HTTP.httpBS req
  --traceShowIO $ HTTP.getResponseBody res
  pure $ HTTP.getResponseBody res


parseResponse
  :: HTTP.Response ByteString
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> IO a
parseResponse res parser = do
 let result = do
       value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
       Aeson.Types.parseEither parser value
       
 case result of 
   Left e -> throwString e
   Right status -> pure status
   

data CreateContainerOptions
 = CreateContainerOptions
    { image :: DockerLocal.Image
    , script :: Text
    , volume :: Volume
    }
    
data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq,Show)
  
containerStatus_ :: ContainerID -> IO ContainerStatus
containerStatus_ cID = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of 
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited ( ContainerExitCode code)
          other -> pure $ ContainerOther other
      path = "/v1.41/containers/" <> containerIDtoText cID <> "/json"
      req = HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestMethod "GET"
          & HTTP.setRequestPort 2375
  res <- HTTP.httpBS req
  parseResponse res parser
      
      
createContainer_ :: CreateContainerOptions -> IO ContainerID
createContainer_ options = do
 let image = imageToText options.image
     bind = volumeToText options.volume <> ":/app"
     body = Aeson.object
              [ ("Image", Aeson.toJSON image)
              , ("Tty", Aeson.toJSON True)
              , ("Labels", Aeson.object [("quad","")])
              , ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh")
              , ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script])
              , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c" ])
              , ("WorkingDir", "/app")
              , ("HostConfig", Aeson.object [ ("Binds", Aeson.toJSON [bind]) ] )
              ]
     req = HTTP.defaultRequest
         & HTTP.setRequestPath "v1.41/containers/create"
         & HTTP.setRequestMethod "POST"
         & HTTP.setRequestBodyJSON body
         & HTTP.setRequestPort 2375
     parser = Aeson.withObject "create-container" $ \o -> do
        cID <- o .: "Id"
        pure $ ContainerID cID
         
 res <- HTTP.httpBS req 
 --traceShowIO res
 parseResponse res parser
 
 
newtype ContainerID = ContainerID Text 
  deriving (Eq,Show, Generic, Serialise.Serialise)
  
containerIDtoText :: ContainerID -> Text
containerIDtoText (ContainerID id) = id
 
newtype ContainerExitCode = ContainerExitCode Int
 deriving (Eq,Show, Generic, Serialise.Serialise)

imageToText :: DockerLocal.Image -> Text
imageToText image = image.name <> ":" <> image.tag


startContainer_ :: ContainerID -> IO ()
startContainer_ container = do
 let path = "v1.41/containers/" <> containerIDtoText container <> "/start"
     req = HTTP.defaultRequest
         & HTTP.setRequestPath (encodeUtf8 path)
         & HTTP.setRequestMethod "POST"
         & HTTP.setRequestPort 2375
 void $ HTTP.httpBS req 




data ContainerInfo = ContainerInfo { id :: Text }  deriving Show

instance FromJSON ContainerInfo where
  parseJSON = Aeson.withObject "CI" $ \v -> ContainerInfo
     <$> v .: "Id"

listQUADcontainers :: IO [ContainerID]
listQUADcontainers = do
 let req = HTTP.defaultRequest
         & HTTP.setRequestPath "/v1.41/containers/json?all=true&filters={\"label\":[\"quad\"]}"
         & HTTP.setRequestMethod "GET"
         & HTTP.setRequestPort 2375
         
 res <- HTTP.httpBS req
 let body = HTTP.getResponseBody res
     (Just rawObj) = decodeStrict body :: Maybe [ContainerInfo]
     list = map (\(ContainerInfo { id } ) -> ContainerID id) rawObj
 --traceShowIO list
 return list
 
removeContainers :: [ContainerID] -> IO ()
removeContainers list = do
 let mkreq = \cID -> 
                HTTP.defaultRequest
              & HTTP.setRequestPath (encodeUtf8 $ "/v1.41/containers/" <> containerIDtoText cID)
              & HTTP.setRequestMethod "DELETE"
              & HTTP.setRequestPort 2375
     reqs = map mkreq list
 res <- mapM HTTP.httpBS reqs
 --traceShowIO res
 pure ()
 
 
newtype Volume = Volume Text
 deriving (Eq,Show, Generic, Serialise.Serialise)
 
volumeToText :: Volume -> Text
volumeToText (Volume text) = text


data FetchLogsOptions
  = FetchLogsOptions
     { container :: ContainerID
     , since :: Time.POSIXTime
     , until :: Time.POSIXTime
     }