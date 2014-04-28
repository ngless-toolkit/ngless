module WebServer 
	(
		runWebServer
	) where

import Happstack.Server ( Browsing(EnableBrowsing), nullConf
                        , serveDirectory, simpleHTTP
                        )

import Data.DefaultValues

serverConf :: Int -> Conf
serverConf port = Conf
    { port      = port
    , validator = Nothing
    , logAccess = Nothing
    , timeout   = 30
    }

runWebServer port = defaultDir >>= simpleHTTP (serverConf port) $ serveDirectory EnableBrowsing []