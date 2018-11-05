{-# LANGUAGE OverloadedStrings #-}

module Fio2Hledger.Types (
    Config(..)
  , Command(..)
  , czConfig
  , czkStyle
  ) where

import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Hledger

data Config = Config
  { -- | Add full pretty printed Payment as a comment after transaction
    dumpPretty      :: Bool
    -- | Deduplicate tags containing same values
  , deduplicate     :: Bool
    -- | Rename by remote account name
  , accountMap      :: Map Text Text
    -- | File containing account renaming rules
  , accountMapFile  :: Maybe FilePath
    -- | Top-level account name
  , accountName     :: Text
    -- | Expenses account name
  , accountExpenses :: Text
    -- | Income account name
  , accountIncome   :: Text
    -- | ATM withdrawals account name
  , accountATM      :: Text
    -- | Card payments account name
  , accountCard     :: Text
    -- | Bank fees payments account name
  , accountFees     :: Text
  , accountInterest :: Text
  } deriving (Show)

instance Default Config where
  def = Config {
      dumpPretty      = False
    , deduplicate     = True
    , accountMap      = mempty
    , accountMapFile  = Nothing
    , accountName     = "fio"
    , accountExpenses = "expenses"
    , accountIncome   = "income"
    , accountATM      = "atm"
    , accountCard     = "card"
    , accountFees     = "fees"
    , accountInterest = "interest"
    }

czConfig :: Config
czConfig = def {
    accountExpenses = "výdaje"
  , accountIncome   = "příjem"
  , accountATM      = "bankomat"
  , accountCard     = "karta"
  , accountFees     = "poplatky"
  , accountInterest = "úrok"
  }

data Command
  = Oneshot
  | Init
  | Sample
  | SampleUpdate
  | Update
  deriving (Eq, Show)

czkStyle :: AmountStyle
czkStyle = amountstyle {
      asprecision       = Precision 2
    , ascommodityside   = R
    , ascommodityspaced = True
    }
