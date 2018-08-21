module Lib
    ( 
    ) where

import Control.Concurrent.STM
import Data.Text

import qualified Data.Map as M
import qualified Data.Sequence as S

type Amount = Integer
type Balance = M.Map Currency Amount

-- Note: their names should be unique!
newtype Currency = Currency { currName :: Text }
newtype User = User { userName :: Text }

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
    { ledgerCurrencies :: TVar (S.Seq Currency)
    , ledgerEntries :: TVar (S.Seq DoubleEntry)
    , ledgerUsers :: TVar (S.Seq User)
    }

-- Usecase operations

createCurrency :: Text -> Ledger -> STM (Ledger, Currency)
createCurrency name ledger = undefined

sendMoney :: DoubleEntry -> Ledger -> STM Ledger
sendMoney entry ledger = undefined

inspectLedger :: Ledger -> STM [DoubleEntry]
inspectLedger ledger = undefined

userBalances :: Ledger -> STM (M.Map User Balance)
userBalances ledger = undefined

-- Other operations

createUser :: Text -> Ledger -> STM (Ledger, User)
createUser name ledger = undefined
