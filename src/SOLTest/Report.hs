-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
--
groupByCategory ::
  [TestCaseDefinition] ->
  Map String TestCaseReport ->
  Map String CategoryReport
groupByCategory definitions results =
  Map.foldlWithKey' addOneResult Map.empty results -- Iterate over them and sum up points and result for each category respectfully
  where
    defsByName = makeDefsByName definitions -- Create map of tests by name

    makeDefsByName defs =
      Map.fromList [(tcdName d, d) | d <- defs] -- Create map of tests by name

    -- Processes one test result
    addOneResult acc testName report =
      case Map.lookup testName defsByName of -- Finds test name in map created previously
        Nothing -> acc -- Definition is missing /VUT SKIP :]
        Just def -> -- Definition exists, get old context and update
          let category = tcdCategory def
              points = tcdPoints def
              oldCategoryReport = getCategoryReport acc category -- Old context
              newCategoryReport = updateCategoryReport oldCategoryReport testName report points -- New context updated
           in Map.insert category newCategoryReport acc -- Put back the to acc

    -- Gets the existing report if exists else creates default one
    getCategoryReport acc category =
      Map.findWithDefault
        (CategoryReport 0 0 Map.empty)
        category
        acc

    -- Gets the old context adds the points and sets result
    updateCategoryReport old testName report points =
      CategoryReport
        { crTotalPoints = crTotalPoints old + points,
          crPassedPoints =
            crPassedPoints old
              + if tcrResult report == Passed then points else 0, -- If not passed do not add
          crTestResults = Map.insert testName report (crTestResults old)
        }

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
--
computeStats ::
  -- | Total @.test@ files found on disk.
  Int ->
  -- | Number of successfully parsed tests.
  Int ->
  -- | Number of tests selected after filtering.
  Int ->
  -- | Category reports (Nothing in dry-run mode).
  Maybe (Map String CategoryReport) ->
  TestStats
computeStats foundCount loadedCount selectedCount mCategoryResults =
  let histogram = maybe (computeHistogram Map.empty) computeHistogram mCategoryResults -- Using compute histogram as stated in header
      -- Gather passedTests
      -- CSHARP variant: passedTests = categoryResults.Values .Sum(cat => cat.TestResults.Count(r => r.Result == Passed));
      -- AI Assisted, i was unable to comme up with this inner passedTests function
      passedTests =
        maybe
          0
          ( sum
              . map
                ( Map.size
                    . Map.filter (\r -> tcrResult r == Passed)
                    . crTestResults
                )
              . Map.elems
          )
          mCategoryResults
   in TestStats -- Build TestStats
        { tsFoundTestFiles = foundCount,
          tsLoadedTests = loadedCount,
          tsSelectedTests = selectedCount,
          tsPassedTests = passedTests,
          tsHistogram = histogram
        }

-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
--
-- Note for me: In csharp it should like this
-- var histogram = new Dictionary<string, int>();
--
--for (int i = 0; i < 10; i++)
--{
--    string key = "0." + i;
--    histogram[key] = 0;
--}
-- foreach (var kvp in categories)
-- {
--     var categoryReport = kvp.Value;
--     double rate = (double)categoryReport.PassedPoints / categoryReport.TotalPoints;
--     string bin = RateToBin(rate);
--     histogram[bin] += 1;
-- }

computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram categories =
  let emptyBins = Map.fromList [(k, 0) | k <- bins] -- Fill bins with 0
      -- Adds one category to histogram
      update hist categoryReport =
        let totalTests = Map.size (crTestResults categoryReport) -- Get all tests count
            passedTests = Map.size (Map.filter ((== Passed) . tcrResult) (crTestResults categoryReport)) -- Filterout only passed ones
            bin = rateToBin $
              if totalTests == 0 -- Safety for 0 division
                then 0.0
                else fromIntegral passedTests / fromIntegral totalTests -- Count success rate and save to bin
         in Map.insertWith (+) bin 1 hist -- Increment bin counter and save value of bin
   in foldl update emptyBins (Map.elems categories) -- Iter over all categories
  where
    bins = ["0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9"] -- All types of bins

-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac
