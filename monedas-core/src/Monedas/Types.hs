module Monedas.Types where

import Control.Concurrent.STM
import Data.Scientific
import Data.Text

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Amount = Scientific
type Balance = M.Map Currency Amount
type ConsistencyRule = Ledger -> STM Bool

-- Note: their names should be unique!
newtype Currency = Currency { currName :: Text }
    deriving (Eq, Ord, Show)
newtype User = User { userName :: Text }
    deriving (Eq, Ord, Show)

data Account
    = AccUser User
    | AccIncome  -- money which comes inside the system
    | AccExpense -- money which goes outside the system

data DoubleEntry = DoubleEntry
    { deAccountFrom :: Account
    , deAccountTo :: Account
    , deCurrency :: Currency
    , deAmount :: Amount
    }

data Ledger = Ledger
    { ledgerCurrencies :: TVar (Set.Set Currency)
    , ledgerEntries :: TVar (Seq.Seq DoubleEntry)
    , ledgerUsers :: TVar (Set.Set User)
    }
