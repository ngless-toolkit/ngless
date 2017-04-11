{- Copyright 2013-2016 NGLess Authors
 - License: MIT
 -}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Output
    ( OutputType(..)
    , MappingInfo(..)
    , outputLno'
    , outputListLno
    , outputListLno'
    , setOutputLno
    , outputFQStatistics
    , outputMapStatistics
    , writeOutputJS
    , writeOutputTSV
    ) where

import Text.Printf (printf)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Data.IORef
import Data.Aeson
import Data.Aeson.TH (deriveToJSON, defaultOptions, Options(..))
import Data.Time (getZonedTime, ZonedTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Console.ANSI
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import           Numeric (showFFloat)
import           Control.Arrow (first)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy as BL


import           Data.FastQ (FastQEncoding(..), encodingName)
import qualified Data.FastQ as FQ
import Configuration
import CmdArgs (Verbosity(..))
import NGLess.NGLEnvironment
import NGLess.NGError

data OutputType = TraceOutput | DebugOutput | InfoOutput | ResultOutput | WarningOutput | ErrorOutput
    deriving (Eq, Ord)

instance Show OutputType where
    show TraceOutput = "trace"
    show DebugOutput = "debug"
    show InfoOutput = "info"
    show ResultOutput = "result"
    show WarningOutput = "warning"
    show ErrorOutput = "error"

data OutputLine = OutputLine !Int !OutputType !ZonedTime !String

instance ToJSON OutputLine where
    toJSON (OutputLine lno ot t m) = object
                                        ["lno" .= lno
                                        , "time" .=  formatTime defaultTimeLocale "%a %d-%m-%Y %T" t
                                        , "otype" .= show ot
                                        , "message" .= m
                                        ]


data BPosInfo = BPosInfo
                    { _mean :: !Int
                    , _median :: !Int
                    , _lowerQuartile :: !Int
                    , _upperQuartile :: !Int
                    } deriving (Show)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 1} ''BPosInfo)

data FQInfo = FQInfo
                { fileName :: String
                , scriptLno :: !Int
                , gcContent :: Double
                , encoding :: String
                , numSeqs :: Int
                , seqLength :: (Int,Int)
                , perBaseQ :: [BPosInfo]
                } deriving (Show)

$(deriveToJSON defaultOptions ''FQInfo)

data MappingInfo = MappingInfo
                { mi_lno :: Int
                , mi_inputFile :: FilePath
                , mi_reference :: String
                , mi_totalReads :: !Int
                , mi_totalAligned :: !Int
                , mi_totalUnique :: !Int
                } deriving (Show)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3} ''MappingInfo)

curLine :: IORef (Maybe Int)
{-# NOINLINE curLine #-}
curLine = unsafePerformIO (newIORef Nothing)

savedOutput :: IORef [OutputLine]
{-# NOINLINE savedOutput #-}
savedOutput = unsafePerformIO (newIORef [])

savedFQOutput :: IORef [FQInfo]
{-# NOINLINE savedFQOutput #-}
savedFQOutput = unsafePerformIO (newIORef [])

savedMapOutput :: IORef [MappingInfo]
{-# NOINLINE savedMapOutput #-}
savedMapOutput = unsafePerformIO (newIORef [])

setOutputLno :: Maybe Int -> IO ()
setOutputLno = writeIORef curLine

outputListLno :: OutputType -> Maybe Int -> [String] -> NGLessIO ()
outputListLno ot lno ms = output ot (fromMaybe 0 lno) (concat ms)

outputListLno' :: OutputType -> [String] -> NGLessIO ()
outputListLno' !ot ms = do
    lno <- liftIO $ readIORef curLine
    outputListLno ot lno ms

outputLno' :: OutputType -> String -> NGLessIO ()
outputLno' !ot m = outputListLno' ot [m]

shouldPrint :: Bool -- ^ is terminal
                -> OutputType
                -> Verbosity
                -> Bool
shouldPrint _ TraceOutput _ = False
shouldPrint _      _ Loud = True
shouldPrint False ot Quiet = ot == ErrorOutput
shouldPrint False ot Normal = ot > InfoOutput
shouldPrint True  ot Quiet = ot >= WarningOutput
shouldPrint True  ot Normal = ot >= InfoOutput

output :: OutputType -> Int -> String -> NGLessIO ()
output !ot !lno !msg = do
    isTerm <- liftIO $ hIsTerminalDevice stdout
    verb <- nConfVerbosity <$> nglConfiguration
    traceSet <- nConfTrace <$> nglConfiguration
    colorOpt <- nConfColor <$> nglConfiguration
    let sp = traceSet || shouldPrint isTerm ot verb
        doColor = case colorOpt of
            ForceColor -> True
            NoColor -> False
            AutoColor -> isTerm
    c <- colorFor ot
    liftIO $ do
        t <- getZonedTime
        modifyIORef savedOutput (OutputLine lno ot t msg:)
        when sp $ do
            let st = if doColor
                        then setSGRCode [SetColor Foreground Dull c]
                        else ""
                rst = if doColor
                        then setSGRCode [Reset]
                        else ""
                tformat = if traceSet -- when trace is set, output seconds
                                then "%a %d-%m-%Y %T"
                                else "%a %d-%m-%Y %R"
                tstr = formatTime defaultTimeLocale tformat t
                lineStr = if lno > 0
                                then printf " Line %s" (show lno)
                                else "" :: String
            putStrLn $ printf "%s[%s]%s: %s%s" st tstr lineStr msg rst

colorFor :: OutputType -> NGLessIO Color
colorFor = return . colorFor'
    where
        colorFor' TraceOutput = White
        colorFor' DebugOutput = White
        colorFor' InfoOutput = Blue
        colorFor' ResultOutput = Black
        colorFor' WarningOutput = Yellow
        colorFor' ErrorOutput = Red


encodeBPStats :: FQ.FQStatistics -> [BPosInfo]
encodeBPStats res = map encode1 (FQ.qualityPercentiles res)
    where encode1 (mean, median, lq, uq) = BPosInfo mean median lq uq

outputFQStatistics :: FilePath -> FQ.FQStatistics -> FastQEncoding -> NGLessIO ()
outputFQStatistics fname stats enc = do
    lno' <- liftIO $ readIORef curLine
    let enc'    = encodingName enc
        sSize'  = FQ.seqSize stats
        nSeq'   = FQ.nSeq stats
        gc'     = FQ.gcFraction stats
        st      = encodeBPStats stats
        lno     = fromMaybe 0 lno'
        binfo   = FQInfo fname lno gc' enc' nSeq' sSize' st
    let p s0 s1  = outputListLno' DebugOutput [s0, s1]
    p "Simple Statistics completed for: " fname
    p "Number of base pairs: "      (show $ length (FQ.qualCounts stats))
    p "Encoding is: "               (show enc)
    p "Number of sequences: "   (show $ FQ.nSeq stats)
    liftIO $ modifyIORef savedFQOutput (binfo:)

outputMapStatistics :: MappingInfo -> NGLessIO ()
outputMapStatistics mi@(MappingInfo _ _ ref total aligned unique) = do
        lno <- liftIO $ readIORef curLine
        let out = outputListLno' ResultOutput
        out ["Finished mapping to ", ref]
        out ["Total reads: ", show total]
        out ["Total reads aligned: ", showNumAndPercentage aligned]
        out ["Total reads Unique map: ", showNumAndPercentage unique]
        out ["Total reads Non-Unique map: ", showNumAndPercentage (aligned - unique)]
        liftIO $ modifyIORef savedMapOutput (mi { mi_lno = fromMaybe 0 lno }:)
    where
        showNumAndPercentage :: Int -> String
        showNumAndPercentage v = concat [show v, " [", showFFloat (Just 2) ((fromIntegral (100*v) / fromIntegral total') :: Double) "", "%]"]
        total' = if total /= 0 then total else 1


data InfoLink = HasQCInfo !Int
                | HasStatsInfo !Int
    deriving (Eq, Show)
instance ToJSON InfoLink where
    toJSON (HasQCInfo lno) = object
                                [ "info_type" .= ("has_QCInfo" :: String)
                                , "lno" .= show lno
                                ]
    toJSON (HasStatsInfo lno) = object
                                [ "info_type" .= ("has_StatsInfo" :: String)
                                , "lno" .= show lno
                                ]

data ScriptInfo = ScriptInfo String String [(Maybe InfoLink,T.Text)] deriving (Show, Eq)
instance ToJSON ScriptInfo where
   toJSON (ScriptInfo a b c) = object [ "name" .= a,
                                            "time" .= b,
                                            "script" .= toJSON c ]

wrapScript :: [(Int, T.Text)] -> [FQInfo] -> [Int] -> [(Maybe InfoLink, T.Text)]
wrapScript script tags stats = first annotate <$> script
    where
        annotate i
            | i `elem` (scriptLno <$> tags) = Just (HasQCInfo i)
            | i `elem` stats = Just (HasStatsInfo i)
            | otherwise =  Nothing

writeOutputJS :: FilePath -> FilePath -> T.Text -> IO ()
writeOutputJS fname scriptName script = do
    fullOutput <- reverse <$> readIORef savedOutput
    fqStats <- reverse <$> readIORef savedFQOutput
    mapStats <- reverse <$> readIORef savedMapOutput
    t <- getZonedTime
    let script' = zip [1..] (T.lines script)
        sInfo = ScriptInfo fname (show t) (wrapScript script' fqStats (mi_lno <$> mapStats))
    BL.writeFile fname (BL.concat
                    ["var output = "
                    , encode $ object
                        [ "output" .= fullOutput
                        , "processed" .= sInfo
                        , "fqStats" .= fqStats
                        , "mapStats" .= mapStats
                        , "scriptName" .= scriptName
                        ]
                    ,";\n"])


writeOutputTSV :: FilePath -> FilePath -> IO ()
writeOutputTSV fqStatsFp mapStatsFp = do
        fqStats <- reverse <$> readIORef savedFQOutput
        mapStats <- reverse <$> readIORef savedMapOutput
        BL.writeFile fqStatsFp  . formatTSV fqHeaders $ encodeFQStats <$> fqStats
        BL.writeFile mapStatsFp . formatTSV msHeaders $ encodeMapStats <$> mapStats
    where
        formatTSV :: [String] -> [[String]] -> BL.ByteString
        formatTSV header contents = BL.concat [BL8.intercalate "\t" (BL8.pack <$> header), "\n",
                                    BL8.intercalate "\n" (asTSVline <$> contents), "\n"]
        asTSVline = BL8.intercalate "\t" . map BL8.pack
        fqHeaders                = ["file"  , "encoding", "numSeqs",    "minSeqLen",         "maxSeqLen",        "gcContent"]
        encodeFQStats FQInfo{..} = [fileName,  encoding, show numSeqs, show (fst seqLength), show (snd seqLength), show gcContent]

        msHeaders                      = ["inputFile",    "reference",       "totalReads",       "totalAligned",       "totalUnique"]
        encodeMapStats MappingInfo{..} = [mi_inputFile, mi_reference, show mi_totalReads, show mi_totalAligned, show mi_totalUnique]
