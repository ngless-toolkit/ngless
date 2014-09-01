module Tests.Utils
    ( isError
    , isOk
    , parsetest
    ) where
import Test.HUnit
import Parse

isError (Right _) = assertFailure "error not caught"
isError (Left _) = return ()

isOk m (Left _) = assertFailure m
isOk _ (Right _) = return ()

parsetest = parsengless "test"
