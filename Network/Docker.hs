{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Docker where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Lens
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

-- getDockerImages :: DockerClientOpts -> IO (Maybe [DockerImage])
-- getDockerImages = decodeResponse . _dockerGetQuery "/images/json"

-- createContainer :: DockerClientOpts -> CreateContainerOpts -> IO(Maybe T.Text)
-- createContainer clientOpts createOpts = getElemFromResponse "Id" <$> (_dockerPostQuery "/containers/create" clientOpts createOpts)

-- startContainer :: DockerClientOpts -> String -> StartContainerOpts -> IO(Status)
-- startContainer clientOpts containerId startOpts = (^. responseStatus) <$> _dockerPostQuery (printf "/containers/%s/start" containerId) clientOpts startOpts

stopContainer :: DockerClientOpts -> String -> IO (Status)
stopContainer  clientOpts cid = (^. responseStatus) <$> run (stopContainerM clientOpts (SStopContainerEndpoint cid))

-- killContainer :: DockerClientOpts -> String -> IO (Status)
-- killContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/kill" containerId) clientOpts

-- restartContainer :: DockerClientOpts -> String -> IO (Status)
-- restartContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/restart" containerId) clientOpts

-- pauseContainer :: DockerClientOpts -> String -> IO (Status)
-- pauseContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/pause" containerId) clientOpts

-- unpauseContainer :: DockerClientOpts -> String -> IO (Status)
-- unpauseContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/unpause" containerId) clientOpts

-- getContainerLogsStream :: DockerClientOpts -> String -> IO ()
-- getContainerLogsStream  clientOpts containerId = do
--                 req <- PH.parseUrl (fullUrl clientOpts url)
--                 let req' =  req {PH.method = "GET"}
--                 PH.withManager PH.defaultManagerSettings $ \m  -> PH.withHTTP req' m  $ \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
--         where url = (printf "/containers/%s/logs?stdout=1&stderr=1&follow=1" containerId)

-- getContainerLogs :: DockerClientOpts -> String -> IO (L.ByteString)
-- getContainerLogs  clientOpts containerId = (^. responseBody) <$> _dockerGetQuery url clientOpts
--         where url = (printf "/containers/%s/logs?stdout=1&stderr=1" containerId)

