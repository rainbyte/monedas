module Monedas.Consistency where

import Control.Concurrent.STM

import qualified Data.Map as M
import qualified Data.Set as Set

import Monedas.Core
import Monedas.Types

ruleNoNegativeBalances :: ConsistencyRule
ruleNoNegativeBalances = \ledger -> do
    balances <- userBalances ledger
    let inspectUser (user, balances) =
            flip all (M.toList balances) $ \(currency, balance) ->
                if balance >= 0
                then True
                else error $ "Negative balance for " <> show (user, currency, balance)
    pure $ all inspectUser $ M.toList balances

ruleAllCurrenciesExist :: ConsistencyRule
ruleAllCurrenciesExist = \ledger -> do
    balances <- userBalances ledger
    currencies <- readTVar (ledgerCurrencies ledger)
    let valid currency = currency `Set.member` currencies
        checkBalances bs = all valid $ M.keys bs
    pure $ all checkBalances balances

ruleAllUsersExist :: ConsistencyRule
ruleAllUsersExist = \ledger -> do
    balances <- userBalances ledger
    users <- readTVar (ledgerUsers ledger)
    let valid user = user `Set.member` users
    pure $ all valid $ M.keys balances

setupConsistency :: Ledger -> IO ()
setupConsistency ledger = do
    atomically $ mapM_ setupRule
        [ (ruleNoNegativeBalances, "Balance should not be negative")
        , (ruleAllCurrenciesExist, "Currency does not exist")
        , (ruleAllUsersExist, "User does not exist")
        ]
  where
    setupRule (rule, msg) = alwaysSucceeds $ do
        b <- rule ledger
        if b
        then return ()
        else error msg