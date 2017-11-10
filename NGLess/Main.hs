{- Copyright 2013-2017 NGLess Authors
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
import System.IO (stderr, hPutStrLn)
import Control.Exception (catch, IOException)
import Control.Concurrent (setNumCapabilities)
import System.Console.ANSI (setSGRCode, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..))
import System.Exit (exitSuccess, exitFailure)

import Control.Monad.Trans.Except
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
import Version
import ReferenceDatabases
import Output
import NGLess
import NGLess.NGError
import NGLess.NGLEnvironment
import Modules
import CmdArgs
import FileManagement
import StandardModules.NGLStdlib
import Network
import Hooks
import Utils.Batch
import Utils.Suggestion
import CWL

import qualified BuiltinModules.Argv as ModArgv
import qualified BuiltinModules.Assemble as ModAssemble
import qualified BuiltinModules.Checks as Checks
import qualified BuiltinModules.AsReads as ModAsReads
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
                        hPutStrLn stderr $
                                        "Should Not Occur Error! This probably indicates a bug in ngless.\n" ++
                                        "\tPlease get in touch with the authors with a description of how this happened.\n" ++
                                        "\tIf possible run your script with the --trace flag and post the script and the resulting trace at \n"++
                                        "\t\thttp://github.com/luispedro/ngless/issues\n" ++
                                        "\tor email us at coelho@embl.de."
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
    mStats <- ModQCStats.loadModule ("" :: T.Text)
    mOrfFind <- ModORFFind.loadModule ("" :: T.Text)
    imported <- loadStdlibModules mods
    let loaded = (mReadlines:mOrfFind:mArgv:mAssemble:mA:mChecks:mRemove:mStats:imported)
    forM_ loaded registerModule
    return loaded


headerStr :: String
headerStr = "NGLess v"++versionStr++" (C) NGLess authors\n"++
            "http://ngless.embl.de/\n"++
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
    when (not (experimentalFeatures opts) && isJust (exportJSON opts)) $
        fatalError ("The use of --export-json requires the --experimental-features flag\n"++
                    "This feature may change at any time.\n")
    when (not (experimentalFeatures opts) && isJust (exportCWL opts)) $
        fatalError ("The use of --export-cwl requires the --experimental-features flag\n"++
                    "This feature may change at any time.\n")
    let (fname,reqversion) = case input opts of
                ScriptFilePath fp -> (fp,True)
                InlineScript _ -> ("inline",False)
    case nThreads opts of
        NThreads n -> setNumCapabilities n
        NThreadsAuto -> getNcpus >>= \case
            Just n -> setNumCapabilities n
            Nothing -> fatalError "Could not determine number of CPUs"
    ngltext <- loadScript (input opts) >>= \case
        Right t -> return t
        Left err ->  fatalError err
    let maybe_add_print = (if print_last opts then wrapPrint else return)
    (shouldOutput,odir) <- runNGLessIO "loading and running script" $ do
        updateNglEnvironment (\e -> e { ngleScriptText = ngltext })
        outputConfiguration
        sc' <- runNGLess $ parsengless fname reqversion ngltext >>= maybe_add_print
        updateNglEnvironment (\e -> e {ngleVersion = fromMaybe (T.pack versionStr) (nglVersion <$> nglHeader sc') })
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
        whenJust (exportJSON opts) $ \jsoname -> liftIO $ do
            writeScriptJSON jsoname sc transformed
            exitSuccess
        whenJust (exportCWL opts) $ \cwlname -> liftIO $ do
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
        writeOutputJS (odir </> "output.js") fname ngltext
        writeOutputTSV False (Just $ odir </> "fq.tsv") (Just $ odir </> "mappings.tsv")
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

modeExec (DownloadDemoMode demo) = do
    let known = ["gut-short", "ocean-short"]
    if demo `elem` known
        then do
            let url = "http://vm-lux.embl.de/~coelho/ngless-data/Demos/" ++ demo ++ ".tar.gz"
            runNGLessIO "downloading a demo" $ downloadExpandTar url "."
            putStrLn ("\nDemo downloaded to " ++ demo)
        else do
            hPutStrLn stderr (redColor ++ "Unknown demo '"++ demo ++ "'.\n"++
                                    T.unpack (suggestionMessage (T.pack demo) (T.pack <$> known))++
                                    "Available demos are:\n")
            forM_ known $ hPutStrLn stderr . ("\t- " ++)
            exitFailure

modeExec (PrintPathMode exec) = runNGLessIO "finding internal path" $ do
    path <- case exec of
      "samtools" -> samtoolsBin
      "prodigal" -> prodigalBin
      "megahit" -> megahitBin
      "bwa" -> bwaBin
      _ -> throwSystemError ("Unknown binary " ++ exec ++ ".")
    liftIO $ putStrLn path


main' = do
    let metainfo = fullDesc <> footer foottext <> progDesc "ngless implement the NGLess language"
        foottext = concat [
                            "ngless v", versionStr, "(C) NGLess Authors 2013-2017\n",
                            "For more information:\n",
                            "\thttp://ngless.embl.de/\n",
                            "For comments/discussion:\n",
                            "\thttps://groups.google.com/forum/#!forum/ngless"
                            ]
        versioner =
            (infoOption ("ngless v" ++ versionStr ++ " (release date: " ++  dateStr ++ ")")
                (long "version" <> short 'V' <> help "print version and exit"))
            <*>
            (infoOption versionStr (long "version-short" <> help "print just version string (useful for scripting)"))
            <*>
            (infoOption ("ngless v" ++ versionStr ++ " (release date: " ++  dateStr ++ "; git revision: " ++ gitHashStr ++ "; compilation date: " ++ compilationDateStr ++ "; embedded binaries: " ++ embeddedStr ++ ")")
                (long "version-debug" <> help "print detailed version information"))
            <*>
            (infoOption dateStr (long "date-short" <> help "print just release date string (useful for scripting)"))
    args <- execParser (info (versioner <*> helper <*> nglessArgs) metainfo)
    config <- initConfiguration args
    updateNglEnvironment' (\env -> env { ngleConfiguration = config })
    modeExec (mode args)

main = catch main' $ \e -> do
            putStrLn ("Exiting after internal error. If you can reproduce this issue, please run your script "++
                    "with the --trace flag and report a bug at http://github.com/luispedro/ngless/issues")
            print (e :: IOException)
            exitFailure
