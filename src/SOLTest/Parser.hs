-- | Parsing the SOLtest @.test@ file format.
module SOLTest.Parser
  ( -- * Entry point
    parseTestFile,
    ParseError (..),

    -- * Intermediate types and functions (exposed for testing)
    ParsedHeader (..),
    emptyHeader,
    splitHeaderBody,
    parseHeader,
    parseHeaderLine,
    determineTestType,
  )
where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import SOLTest.Types
  ( TestCaseDefinition (..),
    TestCaseFile
      ( tcfExpectedStdout,
        tcfName,
        tcfStdinFile,
        tcfTestSourcePath
      ),
    TestCaseType (..),
  )

-- ---------------------------------------------------------------------------
-- Intermediate header type
-- ---------------------------------------------------------------------------

-- | Accumulator for the values parsed from a SOLtest header.
data ParsedHeader = ParsedHeader
  { -- | Value from the @***@ line.
    phDescription :: Maybe String,
    -- | Value from the @+++@ line.
    phCategory :: Maybe String,
    -- | Values from all @---@ lines (in order).
    phTags :: [String],
    -- | Value from the @>>>@ line.
    phWeight :: Maybe Int,
    -- | Values from all @!C!@ lines.
    phParserCodes :: [Int],
    -- | Values from all @!I!@ lines.
    phInterpreterCodes :: [Int]
  }
  deriving (Eq, Show)

data ParseError = MalformedHeader String | MissingRequiredField String | CannotDetermineType String
  deriving (Eq, Show)

-- | An empty header with no fields set.
emptyHeader :: ParsedHeader
emptyHeader =
  ParsedHeader
    { phDescription = Nothing,
      phCategory = Nothing,
      phTags = [],
      phWeight = Nothing,
      phParserCodes = [],
      phInterpreterCodes = []
    }

-- ---------------------------------------------------------------------------
-- File splitting
-- ---------------------------------------------------------------------------

-- | Split the contents of a @.test@ file into header lines and body.
--
-- The split point is the __first__ empty line (a line containing only
-- whitespace). Lines before that point are header lines; everything after
-- is the body (source code), joined back together with newlines.
--
-- If there is no empty line, all lines are treated as header lines and the
-- body is empty.
--
-- Note for me: Basically split content by empty row and split to lines header rest put back as content of test
splitHeaderBody :: String -> ([String], String)
splitHeaderBody content =
  let (header, body) = break (all isSpace) (lines content) -- all isSpace will find the empty row and chops with break the list of lines
   in case body of -- Handle if the test is empty, if not rejoin the lines of body
        [] -> (header, "")
        (_ : bodyLines) -> (header, unlines bodyLines)

-- ---------------------------------------------------------------------------
-- Header line parsing
-- ---------------------------------------------------------------------------

-- | Parse a single header line, updating the accumulated 'ParsedHeader'.
--
-- == Supported prefixes
--
-- * @\"*** \"@ – description ('phDescription')
-- * @\"+++ \"@ – category ('phCategory')
-- * @\"--- \"@ – tag (appended to 'phTags')
-- * @\">>> \"@ – weight (integer, 'phWeight')
-- * @\"!C! \"@ – parser exit code (integer, appended to 'phParserCodes')
-- * @\"!I! \"@ – interpreter exit code (integer, appended to 'phInterpreterCodes')
--
-- == Behavior
--
-- * Known prefixes update the corresponding field
-- * Numeric fields are parsed using 'reads'
--     * invalid values → 'Left' with error message
-- * Unknown or unsupported lines are ignored
--
-- == Result
--
-- * 'Right' updated header on success
-- * 'Left' error message on malformed numeric value
--
-- No comment really needed, this is just guards and load of args of headers (just to make Leo happy :koteseni:)
-- If anything goes wrong just return Left with error message else right with parsed value
parseHeaderLine
  :: ParsedHeader   -- ^ Accumulated header state
  -> String         -- ^ Input line
  -> Either String ParsedHeader
parseHeaderLine hdr line
  | "*** " `isPrefixOf` line =
    let val = trim (drop 4 line)
     in Right hdr {phDescription = Just val}
  | "+++ " `isPrefixOf` line =
    let val = trim (drop 4 line)
     in Right hdr {phCategory = Just val}
  | "--- " `isPrefixOf` line =
    let val = trim (drop 4 line)
     in Right hdr {phTags = phTags hdr ++ [val]}
  | ">>> " `isPrefixOf` line =
    let val = trim (drop 4 line)
     in case reads val of
          [(n, "")] -> Right hdr {phWeight = Just n}
          _ -> Left ("invalid >>> value: " ++ val)
  | "!C! " `isPrefixOf` line =
    let val = trim (drop 4 line)
     in case reads val of
          [(n, "")] -> Right hdr {phParserCodes = phParserCodes hdr ++ [n]}
          _ -> Left ("invalid !C! value: " ++ val)
  | "!I! " `isPrefixOf` line =
    let val = trim (drop 4 line)
     in case reads val of
          [(n, "")] -> Right hdr {phInterpreterCodes = phInterpreterCodes hdr ++ [n]}
          _ -> Left ("invalid !I! value: " ++ val)
  | otherwise = Right hdr -- unknown or comment line: VUT skip

-- | Parse all header lines into a 'ParsedHeader'.
--
-- Processes each line in order using 'parseHeaderLine'. Stops and returns
-- 'Left' on the first error.
parseHeader :: [String] -> Either ParseError ParsedHeader
parseHeader = foldl step (Right emptyHeader)
  where
    step (Left err) _ = Left err
    step (Right hdr) line = case parseHeaderLine hdr line of
      Left msg -> Left $ MalformedHeader msg
      Right x -> Right x

-- ---------------------------------------------------------------------------
-- Test type inference
-- ---------------------------------------------------------------------------

-- | Infer the 'TestCaseType' from a 'ParsedHeader'.
--
-- Rules:
--
-- * Has @!C!@ codes and __no__ @!I!@ codes → 'ParseOnly'
-- * Has @!I!@ codes and __no__ @!C!@ codes → 'ExecuteOnly'
-- * Has @!I!@ codes and @!C!@ is either absent or exactly @[0]@ → 'Combined'
-- * Otherwise → 'Left' (cannot determine type)
determineTestType :: ParsedHeader -> Either ParseError TestCaseType
determineTestType hdr =
  case (phParserCodes hdr, phInterpreterCodes hdr) of
    (_ : _, []) ->
      -- Parser codes present, no interpreter codes → PARSE_ONLY
      Right ParseOnly
    ([], _ : _) ->
      -- No parser codes, interpreter codes present → EXECUTE_ONLY
      Right ExecuteOnly
    (cs, _ : _)
      | null cs || cs == [0] ->
        -- Interpreter codes present, parser codes absent or exactly [0] → COMBINED
        Right Combined
      | otherwise ->
        Left $ CannotDetermineType "invalid combination of !C! and !I! codes"
    ([], []) ->
      Left $ CannotDetermineType "no !C! or !I! codes specified"

-- ---------------------------------------------------------------------------
-- Full file parsing
-- ---------------------------------------------------------------------------

-- | Parse the contents of a @.test@ file into a 'TestCaseDefinition'.
--
-- Returns 'Left' with a @ParseError@ value if:
--
-- * The header is malformed (bad exit code value, etc.)
-- * Required fields (@+++@ category, @>>>@ weight) are missing
-- * The test type cannot be determined from the exit code declarations
parseTestFile :: TestCaseFile -> String -> Either ParseError TestCaseDefinition
parseTestFile tcf content = do
  let (hdrLines, body) = splitHeaderBody content
  hdr <- parseHeader hdrLines

  -- Validate required fields
  category <- maybe (Left $ MissingRequiredField "+++ (category)") Right (phCategory hdr)
  weight <- maybe (Left $ MissingRequiredField ">>> (points)") Right (phWeight hdr)

  testType <- determineTestType hdr

  -- Build the exit code fields according to the inferred type
  let (parserCodes, interpCodes) = buildExitCodes testType hdr

  return
    TestCaseDefinition
      { tcdName = tcfName tcf,
        tcdTestSourcePath = tcfTestSourcePath tcf,
        tcdStdinFile = tcfStdinFile tcf,
        tcdExpectedStdoutFile = tcfExpectedStdout tcf,
        tcdTestType = testType,
        tcdDescription = phDescription hdr,
        tcdCategory = category,
        tcdTags = phTags hdr,
        tcdPoints = weight,
        tcdExpectedParserExitCodes = parserCodes,
        tcdExpectedInterpreterExitCodes = interpCodes,
        tcdSourceCode = body
      }

-- | Build the expected exit code lists from the parsed header and inferred type.
--
-- The resulting pair contains:
--
-- * expected parser exit codes
-- * expected interpreter exit codes
--
-- == Behavior by test type
--
-- * 'ParseOnly':
--     * parser exit codes are taken from 'phParserCodes'
--     * interpreter exit codes are 'Nothing'
--
-- * 'ExecuteOnly':
--     * parser exit codes are 'Nothing'
--     * interpreter exit codes are taken from 'phInterpreterCodes'
--
-- * 'Combined':
--     * interpreter exit codes are always taken from 'phInterpreterCodes'
--     * parser exit codes are:
--         * 'Nothing' if no @!C!@ values were provided
--         * 'Just [...]' if one or more @!C!@ values were provided
--
-- == Notes
--
-- For 'Combined' tests, missing parser exit codes mean that parser success
-- (exit code @0@) is assumed implicitly and is not stored explicitly.
-- If @!C! 0@ is present in the header, it is preserved as @Just [0]@.
buildExitCodes
  :: TestCaseType                 -- ^ Inferred test type
  -> ParsedHeader                 -- ^ Parsed header values
  -> (Maybe [Int], Maybe [Int])   -- ^ Expected parser and interpreter exit codes
buildExitCodes testType header =
  case testType of
    ParseOnly -> (Just (phParserCodes header), Nothing) -- For parse we do not expect nothing for execute codes
    ExecuteOnly -> (Nothing, Just (phInterpreterCodes header)) -- For execute we do not expect nothing for parse codes
    Combined ->
      ( if null (phParserCodes header) -- If no codes were present in header then Nothing
          then Nothing
          else Just (phParserCodes header),
        Just (phInterpreterCodes header) -- No if cause codes are mandatory
      )

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Remove leading and trailing whitespace from a string.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
