{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Docker where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Lens
import qualified Data.Text               as T
import           Network.Wreq
-- import           Pipes
-- import qualified Pipes.ByteString        as PB
-- import qualified Pipes.HTTP              as PH


import           Network.Docker.Internal
import           Network.Docker.Types


getDockerVersion :: DockerClientOpts -> IO (Maybe DockerVersion)
getDockerVersion clientOpts = decodeResponse <$> run (getDockerVersionM clientOpts SVersionEndpoint)

listContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
listContainers clientOpts = decodeResponse <$> run (listContainersM clientOpts SListContainersEndpoint)

listImages :: DockerClientOpts -> IO (Maybe [DockerImage])
listImages clientOpts = decodeResponse <$> run (listImagesM clientOpts SListImagesEndpoint)

createContainer :: DockerClientOpts -> CreateContainerOpts -> IO(Maybe T.Text)
createContainer clientOpts createOpts = getElemFromResponse "Id" <$> run (createContainerM clientOpts (SCreateContainerEndpoint) createOpts)

startContainer :: DockerClientOpts -> String -> StartContainerOpts -> IO(Status)
startContainer clientOpts cid startOpts = (^. responseStatus) <$> run (startContainerM clientOpts (SStartContainerEndpoint cid) startOpts)

stopContainer :: DockerClientOpts -> String -> IO (Status)
stopContainer clientOpts cid = (^. responseStatus) <$> run (stopContainerM clientOpts (SStopContainerEndpoint cid))

killContainer :: DockerClientOpts -> String -> IO (Status)
killContainer clientOpts cid = (^. responseStatus) <$>  run (killContainerM clientOpts (SKillContainerEndpoint cid))

restartContainer :: DockerClientOpts -> String -> IO (Status)
restartContainer clientOpts cid = (^. responseStatus) <$> run (restartContainerM clientOpts (SRestartContainerEndpoint cid))

pauseContainer :: DockerClientOpts -> String -> IO (Status)
pauseContainer clientOpts cid = (^. responseStatus) <$> run (pauseContainerM clientOpts (SPauseContainerEndpoint cid))

unpauseContainer :: DockerClientOpts -> String -> IO (Status)
unpauseContainer clientOpts cid = (^. responseStatus) <$> run (unpauseContainerM clientOpts (SUnpauseContainerEndpoint cid))


-- getContainerLogsStream :: DockerClientOpts -> String -> IO ()
-- getContainerLogsStream  clientOpts containerId = do
--                 req <- PH.parseUrl (fullUrl clientOpts url)
--                 let req' =  req {PH.method = "GET"}
--                 PH.withManager PH.defaultManagerSettings $ \m  -> PH.withHTTP req' m  $ \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
--         where url = (printf "/containers/%s/logs?stdout=1&stderr=1&follow=1" containerId)

-- getContainerLogs :: DockerClientOpts -> String -> IO (L.ByteString)
-- getContainerLogs  clientOpts containerId = (^. responseBody) <$> _dockerGetQuery url clientOpts
--         where url = (printf "/containers/%s/logs?stdout=1&stderr=1" containerId)

