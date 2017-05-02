{- Copyright 2013-2017 NGLess Authors
 - License: MIT
 -}
module Main
    ( main
    ) where

{-| #Structure of ngless

 - This document describes the implementation, not the language.

 - Since the scripts are assumed to be small, everything is performed in memory.

 - 1. The whole text is loaded into memory (after UTF-8 decoding).
 - 2. A complete abstract syntax tree is built.
 - 3. The syntax tree is validated. This includes several checks for sanity.
 - 4. The syntax tree is optimised. This step is not currently implemented.
 - 5. The syntax tree is interpreted.
-}

import Data.Maybe
import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Concurrent
import Options.Applicative
import System.FilePath
import System.Directory
import System.IO (stderr, hPutStrLn)
import System.Console.ANSI
import System.Exit (exitSuccess, exitFailure)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Interpret
import Validation
import ValidationIO
import Transform
import Language
import Types
import Parse
import Configuration
import ReferenceDatabases
import Output
import NGLess
import NGLess.NGError
import NGLess.NGLEnvironment
import Modules
import CmdArgs
import FileManagement (setupHtmlViewer)
import StandardModules.NGLStdlib
import Network
import Hooks
import Utils.Suggestion

import qualified BuiltinModules.Argv as ModArgv
import qualified BuiltinModules.Assemble as ModAssemble
import qualified BuiltinModules.Checks as Checks
import qualified BuiltinModules.AsReads as ModAsReads
import qualified BuiltinModules.Readlines as Readlines
import qualified BuiltinModules.Remove as Remove


-- | wrapPrint transforms the script by transforming the last expression <expr>
-- into write(<expr>, ofile=STDOUT)
wrapPrint :: Script -> NGLess Script
wrapPrint (Script v sc) = Script v <$> wrap sc
    where
        wrap :: [(Int,Expression)] -> NGLess [(Int,Expression)]
        wrap [] = return []
        wrap [(lno,e)]
            | wrapable e = return [(lno,addPrint e)]
            | otherwise = throwScriptError "Cannot add write() statement at the end of script (the script cannot terminate with a print/write call)"
        wrap (e:es) = wrap es >>= return . (e:)
        addPrint e = FunctionCall (FuncName "write") e [(Variable "ofile", BuiltinConstant (Variable "STDOUT"))] Nothing

        wrapable (FunctionCall (FuncName f) _ _ _)
            | f `elem` ["print", "write"] = False
        wrapable _ = True

redColor :: String
redColor = setSGRCode [SetColor Foreground Dull Red]

fatalError :: String -> IO a
fatalError err = do
    hPutStrLn stderr "Exiting after fatal error:"
    hPutStrLn stderr (redColor ++ err)
    hPutStrLn stderr $ setSGRCode [Reset]
    exitFailure

whenStrictlyNormal act = do
    v <- nConfVerbosity <$> nglConfiguration
    when (v == Normal) act

runNGLessIO :: String -> NGLessIO a -> IO a
runNGLessIO context (NGLessIO act) = runResourceT (runExceptT act) >>= \case
        Left (NGError NoErrorExit _) -> exitSuccess
        Left (NGError etype emsg) -> do
            hPutStrLn stderr ("Exiting after fatal error while " ++ context)
            case etype of
                ShouldNotOccur ->
                        hPutStrLn stderr "Should Not Occur Error! This probably indicates a bug in ngless.\n\tPlease get in touch with the authors <coelho@embl.de> with a description of how this happened."
                ScriptError ->
                        hPutStrLn stderr "Script Error"
                DataError ->
                        hPutStrLn stderr "Data Error (the input data did not conform to ngless' expectations)"
                SystemError ->
                        hPutStrLn stderr "System Error"
                _ ->
                        return ()
            hPutStrLn stderr (redColor ++ emsg)
            hPutStrLn stderr $ setSGRCode [Reset]
            exitFailure
        Right v -> return v

-- | Load both automatically imported modules are user-requested one
loadModules :: [ModInfo] -> NGLessIO [Module]
loadModules mods  = do
    mA <- ModAsReads.loadModule ("" :: T.Text)
    mArgv <- ModArgv.loadModule ("" :: T.Text)
    mAssemble <- ModAssemble.loadModule ("" :: T.Text)
    mReadlines <- Readlines.loadModule ("" :: T.Text)
    mChecks <- Checks.loadModule ("" :: T.Text)
    mRemove <- Remove.loadModule ("" :: T.Text)
    imported <- loadStdlibModules mods
    let loaded = (mReadlines:mArgv:mAssemble:mA:mChecks:mRemove:imported)
    forM_ loaded registerModule
    return loaded


headerStr :: String
headerStr = "NGLess v"++versionStr++" (C) NGLess authors\n"++
            "http://luispedro.github.io/ngless\n"++
            "\n"

formatCitation :: T.Text -> String
formatCitation citation = T.unpack . T.unlines . map T.unwords $ citationLines
    where
        lineMax = 90
        wordsWithPrefix = "-":T.words citation
        citationLines :: [[T.Text]]
        citationLines = groupLines wordsWithPrefix
        groupLines :: [T.Text] -> [[T.Text]]
        groupLines [] = []
        groupLines (w:ws) = groupLines' [w,"\t"] (2 + T.length w) ws
        groupLines' acc _ [] = [reverse acc]
        groupLines' acc n rest@(w:ws)
            | T.length w + n > lineMax = reverse acc:groupLines rest
            | otherwise = groupLines' (w:acc) (n + T.length w) ws


printHeader :: [Module] -> NGLessIO ()
printHeader mods = liftIO $ do
    let citations = mapMaybe modCitation mods
    putStr headerStr
    unless (null citations) $ do
        putStr "When publishing results from this script, please cite the following references:\n\n"
        forM_ citations $ \c ->
            putStrLn (formatCitation c)
        putStr "\n"

loadScript :: NGLessInput -> IO (Either String T.Text)
loadScript (InlineScript s) = return . Right . T.pack $ s
loadScript (ScriptFilePath "") = return . Left $ "Either a filename (including - for stdin) or a --script argument must be given to ngless"
loadScript (ScriptFilePath fname) =
        --Note that the input for ngless is always UTF-8.
        --Always. This means that we cannot use T.readFile
        --which is locale aware.
        --We also assume that the text file is quite small and, therefore, loading
        --it in to memory is not resource intensive.
        if fname == "-"
            then decodeUtf8'' <$> B.getContents
            else checkFileReadable fname >>= \case
                Nothing -> decodeUtf8'' <$> B.readFile fname
                Just m -> return $! Left (T.unpack m)
    where
        decodeUtf8'' s = case T.decodeUtf8' s of
            Right r -> Right r
            Left err -> Left (show err)

modeExec :: NGLessMode -> IO ()
modeExec opts@DefaultMode{} = do
    let (fname,reqversion) = case input opts of
                ScriptFilePath fp -> (fp,True)
                InlineScript _ -> ("inline",False)
    setNumCapabilities (nThreads opts)
    ngltext <- loadScript (input opts) >>= \case
        Right t -> return t
        Left err ->  fatalError err
    let maybe_add_print = (if print_last opts then wrapPrint else return)
    (shouldOutput,odir) <- runNGLessIO "loading and running script" $ do
        updateNglEnvironment (\e -> e { ngleScriptText = ngltext })
        sc' <- runNGLess $ parsengless fname reqversion ngltext >>= maybe_add_print
        when (debug_mode opts == "ast") $ liftIO $ do
            forM_ (nglBody sc') $ \(lno,e) ->
                putStrLn ((if lno < 10 then " " else "")++show lno++": "++show e)
            exitSuccess
        outputListLno' DebugOutput ["Loading modules..."]
        modules <- loadModules (fromMaybe [] (nglModules <$> nglHeader sc'))
        sc <- runNGLess $ checktypes modules sc' >>= validate modules
        when (uses_STDOUT `any` [e | (_,e) <- nglBody sc]) $
            whenStrictlyNormal setQuiet
        shouldOutput <- nConfCreateReportDirectory <$> nglConfiguration
        shouldPrintHeader <- nConfPrintHeader <$> nglConfiguration
        outputListLno' DebugOutput ["Validating script..."]
        errs <- validateIO modules sc
        when (isJust errs) $ do
            let errormessage = T.intercalate "\n\n" (fromJust errs)
            liftIO $ fatalError (T.unpack errormessage)
        when shouldPrintHeader $
            printHeader modules
        when (validateOnly opts) $ do
            outputListLno' InfoOutput ["Script OK."]
            liftIO exitSuccess
        outputListLno' TraceOutput ["Transforming script..."]
        when (debug_mode opts == "transform") $
            liftIO (print sc)
        transformed <- transform modules sc
        when (debug_mode opts == "transform") $
            liftIO (print transformed)
        outputListLno' InfoOutput ["Script OK. Starting interpretation..."]
        interpret modules (nglBody transformed)
        triggerHook FinishOkHook
        odir <- nConfReportDirectory <$> nglConfiguration
        return (shouldOutput, odir)
    when shouldOutput $ do
        createDirectoryIfMissing False odir
        setupHtmlViewer odir
        T.writeFile (odir </> "script.ngl") ngltext
        writeOutputJS (odir </> "output.js") fname ngltext
        writeOutputTSV (odir </> "fq.tsv") (odir </> "mappings.tsv")
    exitSuccess


-- if user uses the flag -i he will install a Reference Genome to all users
modeExec (InstallGenMode ref)
    | any ((== ref) . refName) builtinReferences = void . runNGLessIO "installing data" $ installData Nothing ref
    | otherwise =
        error (concat ["Reference ", T.unpack ref, " is not a known reference."])

modeExec (CreateReferencePackMode ofile gen mgtf mfunc) = runNGLessIO "creating reference package" $ do
        outputListLno' InfoOutput ["Starting packaging (will download and index genomes)..."]
        createReferencePack ofile gen mgtf mfunc

modeExec (DownloadFileMode url local) = runNGLessIO "download a file" $
    downloadFile url local

modeExec (DownloadDemoMode demo) = runNGLessIO "downloading a demo" $
    if demo `elem` ["gut-short", "ocean-short"]
        then do
            let url = "http://vm-lux.embl.de/~coelho/ngless-data/Demos/" ++ demo ++ ".tar.gz"
            downloadExpandTar url "."
            liftIO $ putStrLn ("\nDemo downloaded to " ++ demo)
        else liftIO $ do
            hPutStrLn stderr (redColor ++ "Unkown demo '"++ demo ++ "'.")
            exitFailure

main = do
    let metainfo = fullDesc <> footer foottext <> progDesc "ngless implement the NGLess language"
        foottext = concat [
                            "ngless v", versionStr, "(C) NGLess Authors 2013-2017\n",
                            "For more information:\n",
                            "\thttp://ngless.rtfd.io/\n",
                            "For comments/discussion:\n",
                            "\thttps://groups.google.com/forum/#!forum/ngless"
                            ]
        versioner =
            (infoOption ("ngless v" ++ versionStr ++ " (release date: " ++  dateStr ++ ")")
                (long "version" <> short 'V' <> help "print version and exit"))
            <*>
            (infoOption versionStr (long "version-short" <> help "print just version string (useful for scripting)"))
            <*>
            (infoOption ("ngless v" ++ versionStr ++ " (release date: " ++  dateStr ++ "; compilation date: " ++ compilationDateStr ++ "; embedded binaries: " ++ embeddedStr ++ ")")
                (long "version-debug" <> help "print detailed version information"))
            <*>
            (infoOption dateStr (long "date-short" <> help "print just release date string (useful for scripting)"))
    args <- execParser (info (versioner <*> helper <*> nglessArgs) metainfo)
    config <- initConfiguration args
    updateNglEnvironment' (\env -> env { ngleConfiguration = config })
    modeExec (mode args)

