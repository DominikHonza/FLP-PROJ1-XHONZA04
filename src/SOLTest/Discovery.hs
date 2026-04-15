-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import Control.Monad (forM)
import SOLTest.Types
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
--
-- Workflow: Foreach path run search, check if its dir if yes check fo tests if any capture them then look for
-- more dirs and run nested search on them and gather their tests. This will give at end List of tests in this dir and
-- multiple lists of tests for each subdir. At end concat so it returns just on huge list of test back to caller.
-- This wf is for recursive, if not recursive skip part for searching in subdirs
--
-- == Behavior
--
-- * For each path in the directory:
--
--     * If it is a directory:
--         * If @recursive@ is enabled → search it recursively
--         * Otherwise → skip it
--
--     * If it is a file:
--         * Check if it has the @.test@ extension
--         * If yes → call 'findCompanionFiles'
--
-- * Finally, all results are concatenated into a single list.
discoverTests ::
  -- | Whether to search subdirectories recursively
  Bool ->
  -- | Root directory to search
  FilePath ->
  -- | Discovered test cases
  IO [TestCaseFile]
discoverTests recursive dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  nested <- forM fullPaths $ \path -> do
    -- Foreach path run nested search
    isDir <- doesDirectoryExist path -- Check if its dir
    if isDir
      then
        if recursive
          then discoverTests True path -- Check subdirs
          else return []
      else do
        -- Check if its file with correct extension
        isFile <- doesFileExist path -- This is maybe overkill i guess
        if isFile && takeExtension path == ".test"
          then do
            t <- findCompanionFiles path -- Look for in, out files so the test has what it needs
            return [t]
          else return []
  return (concat nested) -- Concat so it return List of tests

-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
