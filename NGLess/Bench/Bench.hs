
import Criterion.Main
import Interpret
import Validation
import ValidationNotPure
import Language
import Tokens
import Types
import Parse
import WebServer
import Configuration
import ReferenceDatabases

import Control.Monad
import Control.Applicative
import Control.Concurrent
import System.Console.CmdArgs
import System.Directory

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S


execute :: String -> T.Text -> IO ()
execute fname text = case parsengless fname text >>= validate >>= checktypes of
            Left err -> T.putStrLn err
            Right expr -> (interpret fname text) . nglBody =<< validate_io expr


executeScript :: FilePath -> IO ()
executeScript fname = do
    odir <- outputDirectory fname
    createDirectoryIfMissing False odir
    engltext <- T.decodeUtf8' <$> (if fname == "-" then S.getContents else S.readFile fname)
    case engltext of
        Left err -> print err
        Right ngltext -> execute fname ngltext



main = defaultMain [ 
        bench "annotation-test" (whnfIO . executeScript $ "examples/annotation.ngl")
      ]
