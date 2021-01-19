{-# LANGUAGE PackageImports #-}
{- Copyright 2013-2021 NGLess Authors
 - License: MIT
 -}
module Main
    ( main
    ) where

{-| #Structure of ngless
 -
 - # Basic ngless interpretation workflow:
 -
 - 1. The whole text is loaded into memory (after UTF-8 decoding). Scripts are small.
 - 2. A complete abstract syntax tree is built.
 - 	These steps are implemented in Tokens.hs & Parse.hs
 - 	The AST is represented with types defined in Language.hs
 - 3. Modules are loaded.
 - 	This step is implemented in Main.hs.
 - 	NGless has several implicit modules (i.e., functionality which is
 - 	implemented as a module, but which the user does not have to import).
 - 	See Modules.hs for more information about modules.
 - 4. The syntax tree is validated. This includes several checks for sanity.
 - 	These steps are implemented in Types.hs (basic type validation)
 - 	followed by Validation.hs and ValidationIO.hs
 - 5. The syntax tree is transformed.
 - 	This step is implemented in Transform.hs. This performs simplification
 - 	and optimization of the tree.
 - 6. The syntax tree is interpreted.
 - 	This step is implemented in Interpret.hs
 - 	Most of the work is then performed by functionality inside
 - 	Interpretation/ or in the implicitly (or explicitly) imported modules
 - 	in BuiltinModules or StandardModules
 -
-}

import Data.Maybe
import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Options.Applicative
import System.FilePath
import System.Directory
import Control.Monad.Extra (whenJust)
import System.IO (stdout, stderr, stdin, hPutStrLn, mkTextEncoding, hGetEncoding, Handle, hSetEncoding)
import Control.Exception (catch, try, throwIO, fromException, displayException)
import Control.Concurrent (setNumCapabilities)
import System.Console.ANSI (setSGRCode, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..))
import System.Exit (exitSuccess, exitFailure, ExitCode(..))

import Control.Monad.Trans.Resource

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Interpret
import JSONScript (writeScriptJSON)
import Validation
import ValidationIO
import Transform
import Language
import Types
import Parse
import Configuration
import qualified Version
import ReferenceDatabases
import Output
import NGLess
import NGLess.NGError
import NGLess.NGLEnvironment
import Modules
import qualified CmdArgs
import FileManagement
import StandardModules.NGLStdlib
import Citations (collectCitations)
import Utils.Network
import Hooks (triggerHook, triggerFailHook, Hook(..))
import Utils.Batch (getNcpus)
import Utils.Suggestion
import CWL (writeCWL)

import qualified BuiltinModules.Argv as ModArgv
import qualified BuiltinModules.Assemble as ModAssemble
import qualified BuiltinModules.Checks as Checks
import qualified BuiltinModules.AsReads as ModAsReads
import qualified BuiltinModules.LoadDirectory as ModLoadDirectory
import qualified BuiltinModules.Readlines as Readlines
import qualified BuiltinModules.Remove as Remove
import qualified BuiltinModules.QCStats as ModQCStats
import qualified BuiltinModules.ORFFind as ModORFFind


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


runNGLessIO :: String -> NGLessIO a -> IO a
runNGLessIO context (NGLessIO act) = try (runResourceT act) >>= \case
        Left (NGError NoErrorExit _) -> exitSuccess
        Left (NGError etype emsg) -> do
            triggerFailHook
            hPutStrLn stderr ("Exiting after fatal error while " ++ context)
            case etype of
                ShouldNotOccur ->
                    hPutStrLn stderr $
                        "Should Not Occur Error! This probably indicates a bug in ngless.\n" ++
                        "\tPlease get in touch with the authors with a description of how this happened.\n" ++
                        "\tIf possible run your script with the --trace flag and post the script and the resulting trace at\n"++
                        "\t\thttps://github.com/ngless-toolkit/ngless/issues.\n"
                ScriptError ->
                    hPutStrLn stderr "Script Error (there is likely an error in your script)"
                DataError ->
                    hPutStrLn stderr "Data Error (the input data did not conform to NGLess' expectations)"
                SystemError ->
                    hPutStrLn stderr "System Error (NGLess was not able to access some necessary resource)"
                _ -> return ()
            hPutStrLn stderr (redColor ++ emsg)
            hPutStrLn stderr $ setSGRCode [Reset]
            exitFailure
        Right v -> return v

-- | Load both automatically imported modules are user-requested one
loadModules :: NGLVersion -> [ModInfo] -> NGLessIO [Module]
loadModules av mods  = do
    mA <- ModAsReads.loadModule ("" :: T.Text)
    mArgv <- ModArgv.loadModule ("" :: T.Text)
    mAssemble <- ModAssemble.loadModule ("" :: T.Text)
    mLoadDirectory <- ModLoadDirectory.loadModule ("" :: T.Text)
    mReadlines <- Readlines.loadModule ("" :: T.Text)
    mChecks <- Checks.loadModule ("" :: T.Text)
    mRemove <- Remove.loadModule ("" :: T.Text)
    mStats <- ModQCStats.loadModule ("" :: T.Text)
    mOrfFind <- ModORFFind.loadModule ("0.6" :: T.Text)
    imported <- loadStdlibModules mods
    let loaded = [mOrfFind | av >= NGLVersion 0 6]
                    ++ [mLoadDirectory | av >= NGLVersion 1 2]
                    ++ [mReadlines, mArgv, mAssemble, mA, mChecks, mRemove, mStats] ++ imported
    forM_ loaded registerModule
    return loaded


headerStr :: String
headerStr = "NGLess v"++Version.versionStr++" (C) NGLess authors\n"++
            "https://ngless.embl.de/\n"++
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


printHeader :: [T.Text] -> NGLessIO ()
printHeader citations = liftIO $ do
    putStr headerStr
    unless (null citations) $ do
        putStr "When publishing results from this script, please cite the following references:\n\n"
        forM_ citations $ \c ->
            putStrLn (formatCitation c)
        putStr "\n"

loadScript :: CmdArgs.NGLessInput -> IO (Either String T.Text)
loadScript (CmdArgs.InlineScript s) = return . Right . T.pack $ s
loadScript (CmdArgs.ScriptFilePath "") = return . Left $ "Either a filename (including - for stdin) or a --script argument must be given to ngless"
loadScript (CmdArgs.ScriptFilePath fname) =
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



modeExec :: CmdArgs.NGLessMode -> IO ()
modeExec opts@CmdArgs.DefaultMode{} = do
    when (not (CmdArgs.experimentalFeatures opts) && isJust (CmdArgs.exportJSON opts)) $
        fatalError ("The use of --export-json requires the --experimental-features flag\n"++
                    "This feature may change at any time.\n")
    when (not (CmdArgs.experimentalFeatures opts) && isJust (CmdArgs.exportCWL opts)) $
        fatalError ("The use of --export-cwl requires the --experimental-features flag\n"++
                    "This feature may change at any time.\n")
    let (fname,reqversion) = case CmdArgs.input opts of
                CmdArgs.ScriptFilePath fp -> (fp,True)
                CmdArgs.InlineScript _ -> ("inline",False)
    case CmdArgs.nThreads opts of
        CmdArgs.NThreads n -> setNumCapabilities n
        CmdArgs.NThreadsAuto -> getNcpus >>= \case
            Just n -> setNumCapabilities n
            Nothing -> fatalError "Could not determine number of CPUs"
    ngltext <- loadScript (CmdArgs.input opts) >>= \case
        Right t -> return t
        Left err ->  fatalError err
    let maybe_add_print = (if CmdArgs.print_last opts then wrapPrint else return)
    (shouldOutput,odir) <- runNGLessIO "loading and running script" $ do
        updateNglEnvironment (\e -> e { ngleScriptText = ngltext })
        outputConfiguration
        sc' <- runNGLess $ parsengless fname reqversion ngltext >>= maybe_add_print
        activeVersion <- runNGLess . parseVersion $ (nglVersion <$> nglHeader sc')
        when (activeVersion < NGLVersion 1 0) $
            outputListLno' WarningOutput ["Using old version (in compatibility mode). If possible, upgrade your version statement to ngless \"1.0\"."]
        updateNglEnvironment (\e -> e {ngleVersion = activeVersion })
        when (CmdArgs.debug_mode opts == "ast") $ liftIO $ do
            forM_ (nglBody sc') $ \(lno,e) ->
                putStrLn ((if lno < 10 then " " else "")++show lno++": "++show e)
            exitSuccess
        outputListLno' DebugOutput ["Loading modules..."]
        modules <- loadModules activeVersion (fromMaybe [] (nglModules <$> nglHeader sc'))
        sc <- runNGLess $ checktypes modules sc' >>= validate modules
        when (uses_STDOUT `any` [e | (_,e) <- nglBody sc]) $ do
            outputListLno' DebugOutput ["Setting quiet mode as STDOUT is used"]
            setQuiet
        shouldOutput <- nConfCreateReportDirectory <$> nglConfiguration
        shouldPrintHeader <- nConfPrintHeader <$> nglConfiguration
        outputListLno' DebugOutput ["Validating script..."]
        errs <- validateIO modules sc
        when (isJust errs) $ do
            let errormessage = T.intercalate "\n\n" (fromJust errs)
            liftIO $ fatalError (T.unpack errormessage)
        when (CmdArgs.validateOnly opts) $ do
            outputListLno' InfoOutput ["Script OK."]
            liftIO exitSuccess
        outputListLno' TraceOutput ["Transforming script..."]
        when (CmdArgs.debug_mode opts == "transform") $
            liftIO (print sc)
        transformed <- transform modules sc
        when (CmdArgs.debug_mode opts == "transform") $
            liftIO (print transformed)
        when shouldPrintHeader $
            printHeader (collectCitations modules transformed)
        whenJust (CmdArgs.exportJSON opts) $ \jsoname -> liftIO $ do
            writeScriptJSON jsoname sc transformed
            exitSuccess
        whenJust (CmdArgs.exportCWL opts) $ \cwlname -> liftIO $ do
            writeCWL sc fname cwlname
            exitSuccess
        outputListLno' InfoOutput ["Script OK. Starting interpretation..."]
        interpret modules (nglBody transformed)
        triggerHook FinishOkHook
        odir <- nConfReportDirectory <$> nglConfiguration
        return (shouldOutput, odir)
    when shouldOutput $ do
        createDirectoryIfMissing False odir
        setupHtmlViewer odir
        T.writeFile (odir </> "script.ngl") ngltext
        writeOutputJSImages odir fname ngltext
        writeOutputTSV False (Just $ odir </> "fq.tsv") (Just $ odir </> "mappings.tsv")
    exitSuccess


-- if user uses the flag -i he will install a Reference Genome to all users
modeExec (CmdArgs.InstallGenMode ref)
    | isBuiltinReference ref = void . runNGLessIO "installing data" $ installData Nothing ref
    | otherwise =
        error (concat ["Reference ", T.unpack ref, " is not a known reference."])

modeExec (CmdArgs.CreateReferencePackMode ofile gen mgtf mfunc) = runNGLessIO "creating reference package" $ do
        outputListLno' InfoOutput ["Starting packaging (will download and index genomes)..."]
        createReferencePack ofile gen mgtf mfunc

modeExec (CmdArgs.DownloadFileMode url local) = runNGLessIO "download a file" $
    downloadFile url local

modeExec (CmdArgs.DownloadDemoMode demo) = do
    let known = ["gut-short", "ocean-short"]
    if demo `elem` known
        then do
            let url = "https://ngless.embl.de/resources/Demos/" ++ demo ++ ".tar.gz"
            runNGLessIO "downloading a demo" $ downloadExpandTar url "."
            putStrLn ("\nDemo downloaded to " ++ demo)
        else do
            hPutStrLn stderr (redColor ++ "Unknown demo '"++ demo ++ "'.\n"++
                                    T.unpack (suggestionMessage (T.pack demo) (T.pack <$> known))++
                                    "Available demos are:\n")
            forM_ known $ hPutStrLn stderr . ("\t- " ++)
            exitFailure

modeExec (CmdArgs.PrintPathMode exec) = runNGLessIO "finding internal path" $ do
    path <- case exec of
      "samtools" -> samtoolsBin
      "prodigal" -> prodigalBin
      "megahit" -> megahitBin
      "bwa" -> bwaBin
      _ -> throwSystemError ("Unknown binary " ++ exec ++ ".")
    liftIO $ putStrLn path

modeExec (CmdArgs.CheckInstallMode verbose) = runNGLessIO "Checking install" $ do
    let checkPath tool pathA
            | verbose = do
                path <- pathA
                liftIO $ putStrLn (tool ++ ": `" ++ path ++ "`")
            | otherwise = void pathA
    checkPath "samtools" samtoolsBin
    checkPath "prodigal" prodigalBin
    checkPath "megahit" megahitBin
    checkPath "bwa" bwaBin
    liftIO $ putStrLn "Install OK"

main' = do
    let metainfo = fullDesc <> footer foottext <> progDesc "ngless implement the NGLess language"
        foottext = concat [
                            "ngless v", Version.versionStr, "(C) NGLess Authors 2013-2020\n",
                            "For more information:\n",
                            "\thttps://ngless.embl.de/\n",
                            "For comments/discussion:\n",
                            "\thttps://groups.google.com/forum/#!forum/ngless\n",
                            "Citation: LP Coelho et al., 2019. ",
                            "https://doi.org/10.1186/s40168-019-0684-8.\n"
                            ]
        versioner =
            (infoOption ("ngless v" ++ Version.versionStr ++ " (release date: " ++ Version.dateStr ++ ")")
                (long "version" <> short 'V' <> help "print version and exit"))
            <*>
            (infoOption Version.versionStr (long "version-short" <> help "print just version string (useful for scripting)"))
            <*>
            (infoOption ("ngless v" ++ Version.versionStr ++ " (release date: " ++ Version.dateStr ++ "; git revision: " ++ Version.gitHashStr ++ "; compilation date: " ++ Version.compilationDateStr ++ "; embedded binaries: " ++ Version.embeddedStr ++ ")")
                (long "version-debug" <> help "print detailed version information"))
            <*>
            (infoOption Version.dateStr (long "date-short" <> help "print just release date string (useful for scripting)"))
    args <- execParser (info (versioner <*> helper <*> CmdArgs.nglessArgs) metainfo)
    config <- initConfiguration args
    updateNglEnvironment' (\env -> env { ngleConfiguration = config })
    modeExec (CmdArgs.mode args)

makeEncodingSafe :: Handle -> IO ()
makeEncodingSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding (takeWhile (/= '/') (show ce) ++ "//TRANSLIT") >>=
      hSetEncoding h

main = do
    mapM_ makeEncodingSafe [stdout, stdin, stderr]
    catch main' $ \e -> case fromException e of
        Just ec -> throwIO (ec :: ExitCode) -- rethrow
        Nothing ->
            fatalError ("An unhandled error occurred (this should not happen)!\n\n" ++
                        "\tIf you can reproduce this issue, please run your script\n" ++
                        "\twith the --trace flag and report a bug (including the script and the trace) at\n" ++
                        "\t\thttps://github.com/ngless-toolkit/ngless/issues\n\n" ++
                        "The error message was: `" ++ displayException e ++ "`")
