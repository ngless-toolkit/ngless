module Tests.Utils
    ( isError
    , isOk
    , parsetest
    , fromRight
    ) where
import Test.HUnit
import Parse

isError (Right _) = assertFailure "error not caught"
isError (Left _) = return ()

isOk m (Left _) = assertFailure m
isOk _ (Right _) = return ()

parsetest = parsengless "test"

fromRight (Right r) = r
fromRight (Left e) = error (concat ["Unexpected Left: ",show e])
