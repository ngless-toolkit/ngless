module WebServer (runWebServer) where

import Happstack.Server.Internal.Types
import Happstack.Server 

import Control.Monad
import Control.Applicative

import Data.Maybe
import Configuration

import System.FilePath.Posix

runWebServer :: FilePath -> Int -> IO ()
runWebServer fname nglessport = do
        putStrLn "Launching Webserver."
        putStrLn ("You can access it at: http://localhost:" ++ show nglessport)
        ddir <- if fname == "-" 
                    then htmlResourcePath
                    else outputDirectory fname
        simpleHTTP serverConf $ nglessApp ddir
    where
        serverConf = nullConf { port = nglessport }



nglessApp :: FilePath -> ServerPart Response
nglessApp ddir = msum
        [ dir "removeDS" $ queryParams "id" >>= ok . toResponse . fromJust
        , dir "serveF" $ queryParams "id" >>= serveFile (guessContentTypeM mimeTypes) . ((</>) ddir) . fromJust
        , serveDirectory EnableBrowsing ["nglessKeeper.html"] ddir]

queryParams param = optional $ look param

