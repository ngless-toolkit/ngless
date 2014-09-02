module WebServer (runWebServer) where

import Happstack.Server.Internal.Types
import Happstack.Server 

import Control.Monad
import Control.Applicative

import Data.Maybe
import Configuration


runWebServer nglessport = do
        putStrLn "Launching Webserver."
        putStrLn ("You can access it at: http://localhost:" ++ show nglessport)
        ddir <- outputDirectory
        simpleHTTP serverConf $ nglessApp ddir
    where
        serverConf = nullConf { port = nglessport }



nglessApp :: String -> ServerPart Response
nglessApp ddir = msum
        [ dir "removeDS" $ queryParams "id" >>= ok . toResponse . fromJust
        , dir "serveF" $ queryParams "id" >>= serveFile (guessContentTypeM mimeTypes) . fromJust
        , serveDirectory EnableBrowsing ["nglessKeeper.html"] ddir]

queryParams param = optional $ look param

