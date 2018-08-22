{-# LANGUAGE RecordWildCards #-}

module Lib
    ( 
    ) where

import Control.Concurrent.STM
import Data.Foldable
import Data.Text

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Amount = Integer
type Balance = M.Map Currency Amount

-- Note: their names should be unique!
newtype Currency = Currency { currName :: Text }
    deriving (Eq, Ord)
newtype User = User { userName :: Text }
    deriving (Eq, Ord)

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

-- Usecase operations

createCurrency :: Text -> Ledger -> STM Currency
createCurrency name ledger = do
    modifyTVar' (ledgerCurrencies ledger) (createCurrency' currency)
    pure currency
  where
    createCurrency' :: Currency -> Set.Set Currency -> Set.Set Currency
    createCurrency' = Set.insert
    currency :: Currency
    currency = Currency { currName = name }

sendMoney :: DoubleEntry -> Ledger -> STM ()
sendMoney entry ledger =
    modifyTVar' (ledgerEntries ledger) (Seq.|> entry)

inspectLedger :: Ledger -> STM (Seq.Seq DoubleEntry)
inspectLedger ledger = do
    entries <- readTVar (ledgerEntries ledger)
    pure entries

userBalances :: Ledger -> STM (M.Map User Balance)
userBalances ledger = do
    entries <- readTVar (ledgerEntries ledger)
    pure $ sumDeltas $ Data.Foldable.concatMap entryDeltas entries
  where
    entryDeltas :: DoubleEntry -> [(User, Currency, Amount)]
    entryDeltas DoubleEntry {..} =
        case (deAccountFrom, deAccountTo) of
            (AccUser userFrom, AccUser userTo) ->
                [ (userFrom, deCurrency, -deAmount)
                , (userTo  , deCurrency,  deAmount)
                ]
            (AccUser userFrom, _) ->
                [ (userFrom, deCurrency, -deAmount) ]
            (_, AccUser userTo) ->
                [ (userTo  , deCurrency,  deAmount) ]
    sumDeltas
        :: (Traversable t)
        => t (User, Currency, Amount) -> M.Map User Balance
    sumDeltas deltas =
        let deltas' = fmap (\(account, b, c) -> (account, M.singleton b c)) deltas
        in M.fromListWith (M.unionWith (+)) $ toList deltas'

-- Other operations

createUser :: Text -> Ledger -> STM User
createUser name ledger = do
    modifyTVar' (ledgerUsers ledger) (createUser' user)
    pure user
  where
    createUser' :: User -> Set.Set User -> Set.Set User
    createUser' = Set.insert
    user :: User
    user = User { userName = name }