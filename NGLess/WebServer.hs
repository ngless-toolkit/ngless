module WebServer (runWebServer) where

import Happstack.Server.Internal.Types
import Happstack.Server 

import Control.Monad
import Control.Applicative

import Data.DefaultValues

serverConf :: Int -> Conf
serverConf p = nullConf { port = p }

runWebServer port = do
    putStrLn $ "Launching WebServer at: " ++ (show port)
    putStrLn $ "You can acess it at: http://localhost:" ++ (show port)
    defaultDir >>= \x -> simpleHTTP (serverConf port) $ myApp x


myApp :: String -> ServerPart Response
myApp x = msum [ dir "removeDS" $ queryParams "id",
				serveDirectory EnableBrowsing ["nglessKeeper.html"] x ]



queryParams :: String -> ServerPart Response
queryParams param =
     do mFoo <- optional $ lookText param
        ok $ (toResponse (show mFoo))