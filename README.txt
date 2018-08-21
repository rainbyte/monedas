This software implements a system which simulates money transactions

-- Main operations

createCurrency :: String -> Ledger -> Ledger
sendMoney :: Entry -> Ledger -> Ledger
inspectLedger :: Ledger -> [Entry]
accountBalance :: User -> Ledger -> Balance

-- Restrictions

-- - Only an user could manage its account
-- - Validate API data (at frontend & backend)
-- - Concurrency should be supported, avoid double-spend
