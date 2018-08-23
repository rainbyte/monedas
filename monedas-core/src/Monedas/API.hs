{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Monedas.API where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Text

import Control.Concurrent.STM

import GHC.Generics

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

import Monedas.Consistency
import Monedas.Core
import Monedas.Types

server_port = 5000

type HandlerT a = (?req :: Request, ?response :: (Response -> IO ResponseReceived), ?state :: Ledger) => a -> IO ResponseReceived
type Handler = HandlerT ()

data ReqCreateCurrency = ReqCreateCurrency { name :: Text }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResCreateCurrency = ResCreateCurrency
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReqSendMoney = ReqSendMoney
    { from :: Text
    , to :: Text
    , currency :: Text
    , amount :: Amount
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResSendMoney = ResSendMoney
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Entry = Entry
    { from :: Text
    , to :: Text
    , currency :: Text
    , amount :: Amount
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ReqInspectLedger = ReqInspectLedger
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResInspectLedger = ResInspectLedger { entries :: [Entry] }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReqUserBalances = ReqUserBalances
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResUserBalances = ResUserBalances
    { balances :: M.Map Text (M.Map Text Amount) }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)


data ReqCreateUser = ReqCreateUser { name :: Text }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResCreateUser = ResCreateUser
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReqCurrencies = ReqCurrencies
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResCurrencies = ResCurrencies { currencies :: [Text] }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReqDeposit = ReqDeposit
    { to :: Text
    , currency :: Text
    , amount :: Amount
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResDeposit = ResDeposit
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReqExtract = ReqExtract
    { from :: Text
    , currency :: Text
    , amount :: Amount
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResExtract = ResExtract
    deriving (Eq, Show, Generic, FromJSON, ToJSON)


data ReqUsers = ReqUsers
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
data ResUsers = ResUsers { users :: [Text] }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)


-- usecase operations

apiCreateCurrency :: HandlerT ReqCreateCurrency
apiCreateCurrency ReqCreateCurrency {..} = do
    let ledger = ?state
    atomically $ createCurrency name ledger
    ?response $ W.responseLBS status200 [] $ encode $ ResCreateCurrency

apiSendMoney :: HandlerT ReqSendMoney
apiSendMoney ReqSendMoney {..} = do
    let entry = DoubleEntry
            { deAccountFrom = AccUser (User {userName = from})
            , deAccountTo = AccUser (User {userName = to})
            , deCurrency = Currency {currName = currency}
            , deAmount = amount
            }
    atomically $ sendMoney entry ?state
    ?response $ W.responseLBS status200 [] $ encode $ ResSendMoney

apiInspectLedger :: HandlerT ReqInspectLedger
apiInspectLedger _ = do
    entries <- atomically $ inspectLedger ?state
    let entries' = toList $ fmap toSimplifiedEntry entries
    ?response $ W.responseLBS status200 [] $ encode $
        ResInspectLedger { entries = entries' }
  where
    toSimplifiedEntry DoubleEntry {..} = Entry
        { from = case deAccountFrom of
            AccUser user -> userName user
            AccIncome -> "#Income"
            _ -> "Wrong account"
        , to = case deAccountTo of
            AccUser user -> userName user
            AccExpense -> "#Expense"
            _ -> "Wrong account"
        , currency = currName deCurrency
        , amount = deAmount
        }

apiUserBalances :: HandlerT ReqUserBalances
apiUserBalances _ = do
    balances <- atomically $ userBalances ?state
    let balances' = M.map (M.mapKeys currName) $ M.mapKeys userName balances
    ?response $ W.responseLBS status200 [] $ encode $
        ResUserBalances { balances = balances' }

-- extra operations
apiCreateUser :: HandlerT ReqCreateUser
apiCreateUser ReqCreateUser {..} = do
    let ledger = ?state
    atomically $ createUser name ledger
    ?response $ W.responseLBS status200 [] $ encode $ ResCreateUser

apiCurrencies :: HandlerT ReqCurrencies
apiCurrencies _ = do
    let ledger = ?state
    currencies <- atomically $ readTVar (ledgerCurrencies ledger)
    let currencies' = fmap currName $ toList currencies 
    ?response $ W.responseLBS status200 [] $ encode $
        ResCurrencies { currencies = currencies'}

apiUsers :: HandlerT ReqUsers
apiUsers _ = do
    let ledger = ?state
    users <- atomically $ readTVar (ledgerUsers ledger)
    let users' = fmap userName $ toList users 
    ?response $ W.responseLBS status200 [] $ encode $
        ResUsers { users = users'}

apiDeposit :: HandlerT ReqDeposit
apiDeposit ReqDeposit {..} = do
    let entry = DoubleEntry
            { deAccountFrom = AccIncome
            , deAccountTo = AccUser (User {userName = to})
            , deCurrency = Currency {currName = currency}
            , deAmount = amount
            }
    atomically $ sendMoney entry ?state
    ?response $ W.responseLBS status200 [] $ encode $ ResDeposit

apiExtract :: HandlerT ReqExtract
apiExtract ReqExtract {..} = do
    let entry = DoubleEntry
            { deAccountFrom = AccUser (User {userName = from})
            , deAccountTo = AccExpense
            , deCurrency = Currency {currName = currency}
            , deAmount = amount
            }
    atomically $ sendMoney entry ?state
    ?response $ W.responseLBS status200 [] $ encode $ ResExtract

createLedger :: IO Ledger
createLedger = Ledger
    <$> newTVarIO Set.empty -- currencies
    <*> newTVarIO Seq.empty -- entries
    <*> newTVarIO Set.empty -- users

server :: IO ()
server = do
    state <- createLedger
    setupConsistency state
    putStrLn $ "Listening on port " ++ show server_port
    run server_port $ \req response ->
        let ?req = req
            ?response = response
            ?state = state
        in do
            print req
            body <- strictRequestBody req
            print body
            case (pathInfo req, requestMethod req) of
                ("createCurrency" : _, "POST") -> withParsedRequest body apiCreateCurrency
                ("createUser" : _, "POST") -> withParsedRequest body apiCreateUser
                ("currencies" : _, "GET") ->  apiCurrencies ReqCurrencies
                ("deposit" : _, "POST") -> withParsedRequest body apiDeposit
                ("extract" : _, "POST") -> withParsedRequest body apiExtract
                ("inspectLedger" : _, "GET") -> apiInspectLedger ReqInspectLedger
                ("sendMoney" : _, "POST") -> withParsedRequest body apiSendMoney
                ("users" : _, "GET") ->  apiUsers ReqUsers
                ("userBalances" : _, "GET") ->  apiUserBalances ReqUserBalances
                _ -> response (W.responseLBS status404 [] "Unknown path")

withParsedRequest :: FromJSON a => BSL.ByteString -> HandlerT (a -> IO ResponseReceived)
withParsedRequest bs handler = case decode bs of
    Nothing -> ?response (W.responseLBS status400 [] "Unable to parse")
    Just x -> handler x
