module Tests.Utils
    ( isError
    , isErrorMsg
    , isOk
    , isOkMsg
    , parsetest
    , fromRight
    , asTempFile
    , testNGLessIO
    ) where
import Test.HUnit
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import System.IO

import Language
import Parse
import FileManagement
import NGLess

isError :: Either a b -> Assertion
isError = isErrorMsg "Error not caught"

isErrorMsg :: String -> Either a b -> Assertion
isErrorMsg m (Right _) = assertFailure m
isErrorMsg _ (Left _) = return ()

isOk :: String -> Either a b -> Assertion
isOk m (Left _) = assertFailure m
isOk _ (Right _) = return ()

isOkMsg = isOk

parsetest :: T.Text -> NGLess Script
parsetest = parsengless "test" True

fromRight (Right r) = r
fromRight (Left e) = error ("Unexpected Left: " ++ show e)

asTempFile :: B.ByteString -> FilePath -> NGLessIO FilePath
asTempFile sf ext = do
    (fp, h) <- openNGLTempFile "testing" "heredoc" ext
    liftIO $ do
        B.hPut h sf
        hClose h
    return fp



