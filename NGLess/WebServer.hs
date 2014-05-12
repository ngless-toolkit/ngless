module WebServer (runWebServer) where

import Happstack.Server.Internal.Types
import Happstack.Server ( Browsing(EnableBrowsing)
                        , serveDirectory, simpleHTTP
                        )

import Data.DefaultValues

serverConf :: Int -> Conf
serverConf p = nullConf { port = p }

runWebServer port = do
    putStrLn $ "Launching WebServer at: " ++ (show port)
    putStrLn $ "You can acess it at: http://localhost:" ++ (show port)
    defaultDir >>= \x -> simpleHTTP (serverConf port) $ serveDirectory EnableBrowsing ["nglessKeeper.html"] x