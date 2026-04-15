-- | Executing test cases by running external parser and interpreter processes.
--
-- Each test case is executed according to its 'TestCaseType':
--
-- * 'ParseOnly': run the parser with source on stdin, check exit code.
-- * 'ExecuteOnly': write XML to a temp file, run the interpreter, check
--   exit code, optionally diff stdout against @.out@.
-- * 'Combined': run the parser first (must exit 0), write its output to a
--   temp file, then run the interpreter as in 'ExecuteOnly'.
-- 
-- AI USED in checkInterpreterResult 
module SOLTest.Executor
  ( executeTest,
    runParser,
    runInterpreter,
    runDiff,
  )
where

import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe)
import SOLTest.Types
import System.Directory (Permissions, doesFileExist, executable, getPermissions)
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Execute a single test case and return a @TestCaseReport@.
--
-- Returns @Left UnexecutedReason@ when execution cannot proceed (e.g.
-- the required executable is missing or not executable).
executeTest ::
  -- | Path to the parser executable (required for 'ParseOnly' and 'Combined').
  Maybe FilePath ->
  -- | Path to the interpreter executable (required for 'ExecuteOnly' and 'Combined').
  Maybe FilePath ->
  TestCaseDefinition ->
  IO (Either UnexecutedReason TestCaseReport)
executeTest mParser mInterp test =
  case tcdTestType test of
    ParseOnly ->
      withExecutable mParser $ \parserPath ->
        Right <$> executeParseOnly parserPath test
    ExecuteOnly ->
      withExecutable mInterp $ \interpPath ->
        Right <$> executeExecuteOnly interpPath test
    Combined ->
      withExecutable mParser $ \parserPath ->
        withExecutable mInterp $ \interpPath ->
          Right <$> executeCombined parserPath interpPath test

-- ---------------------------------------------------------------------------
-- Per-type execution
-- ---------------------------------------------------------------------------

-- | Execute a 'ParseOnly' test case.
executeParseOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeParseOnly parserPath test = do
  (exitCode, pOut, pErr) <- runParser parserPath (tcdSourceCode test)
  let code = exitCodeToInt exitCode
      result
        | code `elem` expectedCodes = Passed
        | otherwise = ParseFail
      expectedCodes = fromMaybe [] (tcdExpectedParserExitCodes test)
  return
    TestCaseReport
      { tcrResult = result,
        tcrParserExitCode = Just code,
        tcrInterpreterExitCode = Nothing,
        tcrParserStdout = Just pOut,
        tcrParserStderr = Just pErr,
        tcrInterpreterStdout = Nothing,
        tcrInterpreterStderr = Nothing,
        tcrDiffOutput = Nothing
      }

-- | Execute an 'ExecuteOnly' test case.
executeExecuteOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeExecuteOnly interpPath test =
  withTempSource (tcdSourceCode test) $ \tmpPath -> do
    (exitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test)
    let code = exitCodeToInt exitCode
        expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
    (result, diffOut) <- checkInterpreterResult code expectedCodes iOut (tcdExpectedStdoutFile test)
    return
      TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = Nothing,
          tcrInterpreterExitCode = Just code,
          tcrParserStdout = Nothing,
          tcrParserStderr = Nothing,
          tcrInterpreterStdout = Just iOut,
          tcrInterpreterStderr = Just iErr,
          tcrDiffOutput = diffOut
        }

-- | Execute a 'Combined' test case.
--
-- This mode first runs the parser and, if successful, passes its output
-- to the interpreter.
--
-- == Behavior
--
-- * Run parser on the test source
--     * If parsing fails (non-zero exit code):
--         * Return 'ParseFail'
--         * Interpreter is NOT executed
--
-- * If parsing succeeds:
--     * Write parser output to a temporary file
--     * Run interpreter on that file
--     * Compare results using 'checkInterpreterResult'
--
-- == Result
--
-- A 'TestCaseReport' which contains:
--
-- * parser results (always)
-- * interpreter results (only if parsing succeeded)
-- * optional diff output if executable
executeCombined ::
  -- | Parser executable path
  FilePath ->
  -- | Interpreter executable path
  FilePath ->
  -- | Test definition
  TestCaseDefinition ->
  IO TestCaseReport
executeCombined parserPath interpPath test = do
  (pExitCode, pOut, pError) <- runParser parserPath (tcdSourceCode test) -- Run parser
  let pCode = exitCodeToInt pExitCode
  if pCode /= 0 -- If parser Code is not 0, not successfull parsing
    then
      return
        TestCaseReport -- Build Test case report for failed parsing
          { tcrResult = ParseFail,
            tcrParserExitCode = Just pCode,
            tcrInterpreterExitCode = Nothing,
            tcrParserStdout = Just pOut,
            tcrParserStderr = Just pError,
            tcrInterpreterStdout = Nothing,
            tcrInterpreterStderr = Nothing,
            tcrDiffOutput = Nothing
          }
    else withTempSource pOut $ \tmpPath -> do
      (iExit, iOut, iError) <- runInterpreter interpPath tmpPath (tcdStdinFile test) -- Run interpeter
      let iCode = exitCodeToInt iExit
          expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
      (result, diffOut) <- checkInterpreterResult iCode expectedCodes iOut (tcdExpectedStdoutFile test) -- Ru ndiff
      return
        TestCaseReport -- Build Test case report for execution
          { tcrResult = result,
            tcrParserExitCode = Just pCode,
            tcrInterpreterExitCode = Just iCode,
            tcrParserStdout = Just pOut,
            tcrParserStderr = Just pError,
            tcrInterpreterStdout = Just iOut,
            tcrInterpreterStderr = Just iError,
            tcrDiffOutput = diffOut
          }

-- ---------------------------------------------------------------------------
-- Process wrappers
-- ---------------------------------------------------------------------------

-- | Run the SOL26 parser by feeding @sourceCode@ on its stdin.
--
-- Returns @(exitCode, stdout, stderr)@.
runParser :: FilePath -> String -> IO (ExitCode, String, String)
runParser parserPath = readCreateProcessWithExitCode (proc parserPath [])

-- | Run the interpreter with @--source \<xmlFile\>@ and, optionally,
-- @--input \<stdinFile\>@.
--
-- Returns @(exitCode, stdout, stderr)@.
runInterpreter ::
  FilePath ->
  FilePath ->
  Maybe FilePath ->
  IO (ExitCode, String, String)
runInterpreter interpPath xmlFile mInputFile = do
  let args = ["--source", xmlFile] ++ maybe [] (\f -> ["--input", f]) mInputFile
  readCreateProcessWithExitCode (proc interpPath args) ""

-- | Run GNU @diff@ between two files (no additional flags).
--
-- Returns @(exitCode, diffOutput)@. Exit code 0 means no differences;
-- exit code 1 means differences were found.
runDiff :: FilePath -> FilePath -> IO (ExitCode, String)
runDiff actualFile expectedFile = do
  (exitCode, out, _) <- readCreateProcessWithExitCode (proc "diff" [actualFile, expectedFile]) ""
  return (exitCode, out)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Check the interpreter's result and optionally run diff.
--
-- Runs diff only when the interpreter exited with code 0 AND a @.out@ file
-- is present.
-- Checks whether the interpreter exit code matches expected codes
-- If matches then compares the output with the provided @.out@ file.
--
-- == Behavior
--
-- * If @actualCode@ is NOT in @expectedCodes@:
--     * Return 'IntFail'
--
-- * If @actualCode == 0@:
--     * If no @.out@ file is provided → return 'Passed'
--     * If @.out@ file exists → run 'runDiffOnOutput'
--
-- * If @actualCode /= 0@ but is expected:
--     * Return 'Passed' without diff check
--
-- AI DISCLAIMER:
-- AI Used : Recommended to use guards instead of If then, transformed to guards
checkInterpreterResult ::
  -- | Actual interpreter exit code.
  Int ->
  -- | Expected interpreter exit codes.
  [Int] ->
  -- | Interpreter stdout.
  String ->
  -- | Path to the @.out@ file, if present.
  Maybe FilePath ->
  IO (TestResult, Maybe String)
checkInterpreterResult actualCode expectedCodes iOut mOutFile
  | actualCode `notElem` expectedCodes = return (IntFail, Nothing) -- Return code does not match expected test fail
  | actualCode == 0 =
    case mOutFile of
      Nothing -> return (Passed, Nothing) -- mOutFile is empty no need t run dif
      Just outFile -> runDiffOnOutput iOut outFile -- Run dif
  | otherwise = return (Passed, Nothing) -- Return code is not 0 but is in expected codes, do not check for .out

-- | Write a string to a temporary file and pass its path to an action.
-- The file is deleted when the action returns.
withTempSource :: String -> (FilePath -> IO a) -> IO a
withTempSource content action =
  withSystemTempFile "sol-source.xml" $ \tmpPath tmpHandle -> do
    hPutStr tmpHandle content
    hClose tmpHandle
    action tmpPath

-- | Write the interpreter stdout to a temp file and diff it against @.out@.
--
-- The function writes the interpreter stdout to a tmp file and
-- runs @diff@ against the provided expected output file.
-- The tmp file is deleted when the action returns.
--
-- == Behavior
--
-- * Write @iOut@ to a temporary file
-- * Run 'runDiff' between the temporary file and @outFile@
-- * Evaluate the exit code of @diff@
--
-- == Result
--
-- * 'Passed' if outputs match (diff exit code = success)
-- * 'DiffFail' if outputs differ
--     * includes diff output in 'Just'
runDiffOnOutput ::
  -- | Interpreter stdout
  String ->
  -- | Expected output file
  FilePath ->
  IO (TestResult, Maybe String)
runDiffOnOutput iOut outFile =
  withSystemTempFile "sol-actual.out" $ \tmpPath tmpHandle -> do
    -- Copy paste from withTempSource
    hPutStr tmpHandle iOut
    hClose tmpHandle
    (exitCode, out) <- runDiff tmpPath outFile -- Run diff
    case exitCode of
      ExitSuccess -> return (Passed, Nothing)
      ExitFailure _ -> return (DiffFail, Just out) -- On failure return DiffFail and out produced by diff

-- | Ensure an executable path is provided and the file is executable,
-- then run an action with it.  Returns 'Left' 'CannotExecute' if the
-- path is missing or the file is not executable.
withExecutable ::
  Maybe FilePath ->
  (FilePath -> IO (Either UnexecutedReason TestCaseReport)) ->
  IO (Either UnexecutedReason TestCaseReport)
withExecutable Nothing _ =
  return
    ( Left
        UnexecutedReason
          { urCode = CannotExecute,
            urMessage = Just "Required executable path was not provided"
          }
    )
withExecutable (Just path) action = do
  check <- checkExecutable path
  case check of
    Just reason -> return (Left reason)
    Nothing -> action path

-- | Check that a file exists and has its executable bit set.
--
-- Returns:
--
-- * 'Nothing' if the file exists and has the executable permission set
-- * 'Just' 'UnexecutedReason' if the file cannot be executed
--
-- == Behavior
--
-- * If the file is ok executable returns Nothing
-- * If the file does not exist → returns 'CannotExecute'
-- * If an IO error occurs → returns 'CannotExecute' with the error message
-- * If the file exists but is not executable → returns 'CannotExecute'
checkExecutable ::
  -- | Path to the executable file
  FilePath ->
  IO (Maybe UnexecutedReason)
checkExecutable path = do
  result <- try (doesFileExist path) :: IO (Either IOException Bool)
  case result of
    Left err -> return (Just (UnexecutedReason CannotExecute (Just (show err))))
    -- File with executable does not exist, return Just with reason
    Right False ->
      return
        ( Just
            UnexecutedReason
              { urCode = CannotExecute,
                urMessage = Just ("Executable not found: " ++ path)
              }
        )
    -- File exists, try to get permission, if exec then Nothing else return error reason in Just
    Right True -> do
      permResult <- try (getPermissions path) :: IO (Either IOException Permissions)
      case permResult of
        Left err -> return (Just (UnexecutedReason CannotExecute (Just (show err)))) -- Permissions not granted
        Right perms ->
          if executable perms
            then return Nothing -- Is executable
            else -- Is not executable

              return
                ( Just
                    UnexecutedReason
                      { urCode = CannotExecute,
                        urMessage = Just ("File is not executable: " ++ path)
                      }
                )

-- | Convert 'ExitCode' to an 'Int'.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
