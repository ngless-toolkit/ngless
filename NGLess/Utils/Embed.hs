{-# LANGUAGE TemplateHaskell #-}

module Utils.Embed
        (embedStr
        ,embedFile
        ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Instances.TH.Lift ()
import qualified Data.ByteString as B

embedStr :: IO B.ByteString -> ExpQ
embedStr act = lift =<< (runIO act)

embedFile :: FilePath -> ExpQ
embedFile f = embedStr (B.readFile f)
