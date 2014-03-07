
module InvokeExternalProgs
    ( 
    	indexReference
    ) where

import GHC.Conc
import System.Cmd
import Data.Text as T


indexReference refPath = rawSystem "/tmp/bwa-0.7.7/bwa" ["index", (T.unpack refPath)]

mapToReference refIndex readSet = rawSystem "/tmp/bwa-0.7.7/bwa" []