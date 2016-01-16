module Tests.Utils
    ( isError
    , isErrorMsg
    , isOk
    , isOkMsg
    , isRight
    , parsetest
    , fromRight
    ) where
import Test.HUnit
import qualified Data.Text as T

import Language
import Parse

isError :: Either a b -> Assertion
isError = isErrorMsg "Error not caught"

isErrorMsg :: String -> Either a b -> Assertion
isErrorMsg m (Right _) = assertFailure m
isErrorMsg _ (Left _) = return ()

isOk :: String -> Either a b -> Assertion
isOk m (Left _) = assertFailure m
isOk _ (Right _) = return ()

isOkMsg = isOk

parsetest :: T.Text -> Either T.Text Script
parsetest = parsengless "test" True

fromRight (Right r) = r
fromRight (Left e) = error (concat ["Unexpected Left: ",show e])

isRight (Right _) = True
isRight (Left _) = False

