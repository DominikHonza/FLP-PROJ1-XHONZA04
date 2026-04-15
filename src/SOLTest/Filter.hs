-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec tests =
  let useRegexSpec = fsUseRegex spec -- Load specs for regex
      includesSpecs = fsIncludes spec -- Load specs for include
      excludesSpecs = fsExcludes spec -- Load specs for exclude
      included =
        if null includesSpecs -- If none includeSpec take all tests
          then tests
          else filter (matchesAny useRegexSpec includesSpecs) tests -- Apply criterions to select test to filter on
      selected = filter (not . matchesAny useRegexSpec excludesSpecs) included -- Select final tests
      filteredOut = filter (`notElem` selected) tests -- Save the left out
   in (selected, filteredOut)

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
--
-- == Behavior
--
-- * 'ByAny':
--     * Matches if the value equals:
--         * test name ('tcdName')
--         * test category ('tcdCategory')
--         * any tag in 'tcdTags'
--
-- * 'ByCategory':
--     * Matches if the value equals the test category
--
-- * 'ByTag':
--     * Matches if the value is present in test tags
--
-- * All values are trimmed using 'trimFilterId' before comparison
--
-- == Notes
--
-- * Currently, @useRegex@ is ignored (exact string matching only)
-- * Matching is case-sensitive
matchesCriterion
  :: Bool                 -- ^ Enable regex matching (currently unused)
  -> TestCaseDefinition   -- ^ Test to evaluate
  -> FilterCriterion      -- ^ Criterion to match against
  -> Bool
matchesCriterion _ test criterion =
  case criterion of
    ByAny raw ->
      let valTrimmed = trimFilterId raw
       in tcdName test == valTrimmed
            || tcdCategory test == valTrimmed -- Check if name corresponds to category
            || valTrimmed `elem` tcdTags test -- Check if valTrimmed present in tcdTags
    ByCategory raw -> tcdCategory test == trimFilterId raw -- Just compare category of test with criterium
    ByTag raw -> trimFilterId raw `elem` tcdTags test -- Check if trimFilterId is in tcdTags

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
