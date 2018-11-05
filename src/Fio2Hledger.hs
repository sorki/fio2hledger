{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fio2Hledger where

import Control.Monad
import Data.Default (def)
import API.Fio (AccountStatement, Token)
import Money (SomeDense)

import qualified Data.Bool
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO
import qualified Data.Time.Calendar
import qualified API.Fio
import qualified Hledger
import qualified Money
import qualified System.Directory
import qualified System.Environment

import Fio2Hledger.Format
import Fio2Hledger.Types
import qualified Paths_fio2hledger

import Options.Applicative

parseCommand :: Parser Command
parseCommand = subparser $
       command "oneshot" (info (pure Oneshot) (progDesc "Generate all-in-one journal file"))
   <> command "init" (info (pure Init) (progDesc "Initialize journals"))
   <> command "sample" (info (pure Sample) (progDesc "Generate sample journals"))
   <> command "sample-update" (info (pure SampleUpdate) (progDesc "Update sample journals"))
   <> command "update" (info (pure Update) (progDesc "Update journals with last transactions"))

data Options = Options {
   pretty  :: Bool
 , nodedup :: Bool
 , cz      :: Bool
 , amap    :: Maybe FilePath
 } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
  <$> switch (short 'p' <> long "dump"<> help "Dump full payments into journal as comments")
  <*> switch (short 'n' <> long "nodedup" <> help "Don't deduplicate tags")
  <*> switch (short 'c' <> long "cz" <> help "Use Czech names for accounts")
  <*> optional (option str (short 'm' <> long "map" <> help "Account map file"))

parse :: Parser (Options, Command)
parse = (,)
  <$> parseOptions
  <*> parseCommand

run :: Command -> Config -> IO ()
run Oneshot = oneshot
run Sample = sample
run SampleUpdate = updateSample
run Init = initialize
run Update = update

main :: IO ()
main = do
  (opt, cmd) <- execParser opts
  aMap <- maybe
    (pure mempty)
    parseMap
    (amap opt)

  let
    def' = Data.Bool.bool def czConfig (cz opt)
    cfg = def'
      { dumpPretty = pretty opt    
      , deduplicate = not $ nodedup opt
      , accountMap = aMap
      }

  run cmd cfg
  where
    opts = info (parse <**> helper)
      ( fullDesc
      <> progDesc "fio2hledger")
    parseMap f = do
      fc <- Data.Text.Lazy.IO.readFile f
      return $
        Data.Map.fromList
          $ Data.Maybe.mapMaybe (toKV . Data.Text.words)
          $ Data.Text.lines
          $ Data.Text.Lazy.toStrict fc
      where
        toKV [k, v] = pure (k, v)
        toKV _ = Nothing

oneshot :: Config -> IO ()
oneshot cfg = do
  tokPath <- System.Environment.getEnv "FIO_TOKEN_PATH"

  tok <- API.Fio.tokenFromFile tokPath

  putStrLn "Querying transactions"
  as <- API.Fio.getAllTransactions tok

  let
    currency = Money.someDenseCurrency (API.Fio.openingBalance as)
    filename = Data.Text.unpack . Data.Text.toLower
         $  "fio-"
        <> currency
        <> ".journal"

  putStrLn $ "Writing " ++ filename
  Data.Text.Lazy.IO.writeFile
    filename
    $ Data.Text.Lazy.Builder.toLazyText
        $ formatAccountStatement cfg as

-- | Perform update
-- check span of last, append and create new if needed
-- append loads current journal, checks if we need to add
-- monthly header (last transactions month differs) and appends transaction
update :: Config -> IO ()
update cfg = do
  tok <- getToken
  putStrLn "Querying last transactions"
  as <- API.Fio.getLast tok

  unless (API.Fio.validateSome as)
    $ error "Account statement balance doesn't validate"
  updateInplace cfg as

updateInplace :: Config -> AccountStatement SomeDense -> IO ()
updateInplace cfg as = do
  let
    currency = Money.someDenseCurrency (API.Fio.openingBalance as)

  fs <- forM (groupTransactions cfg as) $ \year -> do
    let
      yearText = formatYear . Hledger.tdate . head . head $ year
      filename = Data.Text.unpack . Data.Text.toLower
         $  "fio-"
        <> (currency <> "-" <> yearText)
        <> ".journal"

    exists <- System.Directory.doesFileExist filename
    if exists then
      (do
        mj <- Hledger.readJournalFile
          Hledger.definputopts
          ("journal:" ++ filename)

        case mj of
          Left e ->
            error
              $ "Unable to read old journal file: "
              ++ filename
              ++ " error was "
              ++ e
          Right j -> do
            let lastTransaction =
                  head
                    $ Hledger.jtxns
                    $ Hledger.journalReverse j

            let
              getM day = let (_, m, _) = Data.Time.Calendar.toGregorian day in m
              lastTrMonth = getM $ Hledger.tdate lastTransaction
              updateTrMonth = getM $ Hledger.tdate $ head $ head year

            putStrLn $ "Appending transactions to " ++ filename
            Data.Text.Lazy.IO.appendFile
              filename
              -- drop first line containing month header when
              -- months are the same as last recorded transaction
              $ Data.Bool.bool
                  id
                  (Data.Text.Lazy.unlines . drop 1 . Data.Text.Lazy.lines)
                  (lastTrMonth == updateTrMonth)
              $ Data.Text.Lazy.Builder.toLazyText
                  $ formatMonthlyTransactions year
            return Nothing)
      else (do
        putStrLn $ "Writing " ++ filename
        Data.Text.Lazy.IO.writeFile
          filename
          $ Data.Text.Lazy.Builder.toLazyText
            $ statementPreamble as
              <> formatYearlyTransactions [year]
        return $ pure filename)

  appendFile
    (Data.Text.unpack $ Data.Text.toLower ("fio-" <> currency <> ".journal"))
    $ unlines
    $ map ("include "<>)
    $ Data.Maybe.catMaybes fs

-- | Create yearly journals and toplevel file that imports them
createYearly :: Config -> AccountStatement SomeDense -> IO ()
createYearly cfg as = do
  fs <- forM (formatAccountStatements cfg as) $ \(year, builder) -> do
    let
      filename = Data.Text.unpack . Data.Text.toLower
         $  "fio-"
        <> (currency <> "-" <> year)
        <> ".journal"

    putStrLn $ "Writing " ++ filename
    Data.Text.Lazy.IO.writeFile
      filename
      $ Data.Text.Lazy.Builder.toLazyText builder

    return filename

  putStrLn "Writing toplevel"
  writeFile
    (Data.Text.unpack
      $ Data.Text.toLower
      ("fio-" <> currency <> ".journal"))
    $ unlines
    $ map ("include "<>) fs
  where
    currency = Money.someDenseCurrency (API.Fio.openingBalance as)

-- | Perform initial import fetching all transactions
-- and saving to journal files grouped by year
initialize :: Config -> IO ()
initialize cfg = do
  tok <- getToken
  -- flush last transactions first
  putStrLn "Flushing last transactions"
  API.Fio.flushLast tok

  putStrLn "Querying transactions"
  as <- API.Fio.getAllTransactions tok

  unless (API.Fio.validateSome as)
    $ error "Account statement balance doesn't validate"
  createYearly cfg as

updateSample :: Config -> IO ()
updateSample cfg = do
  eAs <- getSample "sample-update.json"
  case eAs of
    Left e -> error $ "Unable to decode account statement, error was: " ++ e
    Right as ->
      updateInplace cfg as

sample :: Config -> IO ()
sample cfg = do
  eAs <- getSample "sample.json"

  case eAs of
    Left e -> error $ "Unable to decode account statement, error was: " ++ e
    Right as -> do
      unless (API.Fio.validateSome as)
        $ error "Sample balance doesn't validate"
      Data.Text.Lazy.IO.writeFile
        "fio-czk.journal"
        $ Data.Text.Lazy.Builder.toLazyText
            $ formatAccountStatement sampleConfig as
  where
    sampleConfig =
      cfg
        { accountMap =
            Data.Map.fromList
              [ ("2900133700/2010", "leetCorp"),
                ("2900232323/2010", "someCorp")
              ]
        }

splitSample :: Config -> IO ()
splitSample cfg = do
  eAs <- getSample "sample.json"

  case eAs of
    Left e -> error $ "Unable to decode account statement, error was: " ++ e
    Right as -> do
      unless (API.Fio.validateSome as)
        $ error "Sample balance doesn't validate"

      createYearly cfg as

getToken :: IO Token
getToken =
  System.Environment.getEnv "FIO_TOKEN_PATH"
  >>= API.Fio.tokenFromFile

getSample :: FilePath -> IO (Either String (AccountStatement SomeDense))
getSample f = do
  dir <- Paths_fio2hledger.getDataDir
  API.Fio.eitherDecodeStatementFile
      (dir ++ "/sample-in/" ++ f)
