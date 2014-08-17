module WebServer (runWebServer) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

import Happstack.Server.Internal.Types
import Happstack.Server 

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.DefaultValues
import Data.Json


serverConf :: Int -> Conf
serverConf p = nullConf { port = p }

runWebServer port = do
    putStrLn $ "Launching WebServer at: " ++ (show port)
    putStrLn $ "You can acess it at: http://localhost:" ++ (show port)
    defaultDir >>= \x -> simpleHTTP (serverConf port) $ myApp x


myApp :: String -> ServerPart Response
myApp x = msum [ dir "removeDS" $ queryParams "id" >>= ok . toResponse . fromJust
                 , dir "serveF" $ queryParams "id" >>= serveFile (guessContentTypeM mimeTypes) . fromJust
                 , serveDirectory EnableBrowsing ["nglessKeeper.html"] x ]


serveF :: String -> ServerPart Response
serveF p = serveFile (guessContentTypeM mimeTypes) p

queryParams param = optional $ look param