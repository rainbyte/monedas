{-# LANGUAGE DeriveGeneric #-}
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

data ReqCreateCurrency = ReqCreateCurrency
    { reqCreateCurrency_name :: Text
    }
    deriving (Eq, Show, Generic)
data ResCreateCurrency = ResCreateCurrency
    deriving (Eq, Show, Generic)

data ReqSendMoney = ReqSendMoney
    { reqSendMoney_from :: Text
    , reqSendMoney_to :: Text
    , reqSendMoney_currency :: Text
    , reqSendMoney_amount :: Amount
    }
    deriving (Eq, Show, Generic)
data ResSendMoney = ResSendMoney
    deriving (Eq, Show, Generic)

data Entry = Entry
    { entry_from :: Text
    , entry_to :: Text
    , entry_currency :: Text
    , entry_amount :: Amount
    }
    deriving (Eq, Show, Generic)
data ReqInspectLedger = ReqInspectLedger
    deriving (Eq, Show, Generic)
data ResInspectLedger = ResInspectLedger
    { resInspectLedger_entries :: [Entry] }
    deriving (Eq, Show, Generic)

data ReqUserBalances = ReqUserBalances
    deriving (Eq, Show, Generic)
data ResUserBalances = ResUserBalances
    { resUserBalances_balances :: M.Map Text (M.Map Text Amount) }
    deriving (Eq, Show, Generic)


data ReqCreateUser = ReqCreateUser
    { reqCreateUser_name :: Text }
    deriving (Eq, Show, Generic)
data ResCreateUser = ResCreateUser
    deriving (Eq, Show, Generic)

data ReqCurrencies = ReqCurrencies
    deriving (Eq, Show, Generic)
data ResCurrencies = ResCurrencies
    { resCurrencies_currencies :: [Text] }
    deriving (Eq, Show, Generic)

data ReqDeposit = ReqDeposit
    { reqDeposit_to :: Text
    , reqDeposit_currency :: Text
    , reqDeposit_amount :: Amount
    }
    deriving (Eq, Show, Generic)
data ResDeposit = ResDeposit
    deriving (Eq, Show, Generic)

data ReqExtract = ReqExtract
    { reqExtract_from :: Text
    , reqExtract_currency :: Text
    , reqExtract_amount :: Amount
    }
    deriving (Eq, Show, Generic)
data ResExtract = ResExtract
    deriving (Eq, Show, Generic)


data ReqUsers = ReqUsers
    deriving (Eq, Show, Generic)
data ResUsers = ResUsers
    { resUsers_users :: [Text]
    }
    deriving (Eq, Show, Generic)


-- usecase operations

apiCreateCurrency :: HandlerT ReqCreateCurrency
apiCreateCurrency ReqCreateCurrency {..} = do
    let ledger = ?state
    atomically $ createCurrency reqCreateCurrency_name ledger
    ?response $ W.responseLBS status200 [] $ encode $ ResCreateCurrency

apiSendMoney :: HandlerT ReqSendMoney
apiSendMoney ReqSendMoney {..} = do
    let entry = DoubleEntry
            { deAccountFrom = AccUser (User {userName = reqSendMoney_from})
            , deAccountTo = AccUser (User {userName = reqSendMoney_to})
            , deCurrency = Currency {currName = reqSendMoney_currency}
            , deAmount = reqSendMoney_amount
            }
    atomically $ sendMoney entry ?state
    ?response $ W.responseLBS status200 [] $ encode $ ResSendMoney

apiInspectLedger :: HandlerT ReqInspectLedger
apiInspectLedger _ = do
    entries <- atomically $ inspectLedger ?state
    let entries' = toList $ fmap toSimplifiedEntry entries
    ?response $ W.responseLBS status200 [] $ encode $
        ResInspectLedger { resInspectLedger_entries = entries' }
  where
    toSimplifiedEntry DoubleEntry {..} = Entry
        { entry_from = case deAccountFrom of
            AccUser user -> userName user
            AccIncome -> "#Income"
            _ -> "Wrong account"
        , entry_to =  case deAccountTo of
            AccUser user -> userName user
            AccExpense -> "#Expense"
            _ -> "Wrong account"
        , entry_currency = currName deCurrency
        , entry_amount = deAmount
        }

apiUserBalances :: HandlerT ReqUserBalances
apiUserBalances _ = do
    balances <- atomically $ userBalances ?state
    let balances' = M.map (M.mapKeys currName) $ M.mapKeys userName balances
    ?response $ W.responseLBS status200 [] $ encode $
        ResUserBalances { resUserBalances_balances = balances' }

-- extra operations
apiCreateUser :: HandlerT ReqCreateUser
apiCreateUser ReqCreateUser {..} = do
    let ledger = ?state
    atomically $ createUser reqCreateUser_name ledger
    ?response $ W.responseLBS status200 [] $ encode $ ResCreateUser

apiCurrencies :: HandlerT ReqCurrencies
apiCurrencies _ = do
    let ledger = ?state
    currencies <- atomically $ readTVar (ledgerCurrencies ledger)
    let currencies' = fmap currName $ toList currencies 
    ?response $ W.responseLBS status200 [] $ encode $
        ResCurrencies { resCurrencies_currencies = currencies'}

apiUsers :: HandlerT ReqUsers
apiUsers _ = do
    let ledger = ?state
    users <- atomically $ readTVar (ledgerUsers ledger)
    let users' = fmap userName $ toList users 
    ?response $ W.responseLBS status200 [] $ encode $
        ResUsers { resUsers_users = users'}

apiDeposit :: HandlerT ReqDeposit
apiDeposit ReqDeposit {..} = do
    let entry = DoubleEntry
            { deAccountFrom = AccIncome
            , deAccountTo = AccUser (User {userName = reqDeposit_to})
            , deCurrency = Currency {currName = reqDeposit_currency}
            , deAmount = reqDeposit_amount
            }
    atomically $ sendMoney entry ?state
    ?response $ W.responseLBS status200 [] $ encode $ ResDeposit

apiExtract :: HandlerT ReqExtract
apiExtract ReqExtract {..} = do
    let entry = DoubleEntry
            { deAccountFrom = AccUser (User {userName = reqExtract_from})
            , deAccountTo = AccExpense
            , deCurrency = Currency {currName = reqExtract_currency}
            , deAmount = reqExtract_amount
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

jsonOptions = defaultOptions { fieldLabelModifier = Prelude.tail . Prelude.dropWhile (/= '_') }

instance FromJSON ReqCreateCurrency where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqCreateCurrency where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResCreateCurrency where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResCreateCurrency where
    toJSON = genericToJSON jsonOptions

instance FromJSON ReqCreateUser where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqCreateUser where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResCreateUser where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResCreateUser where
    toJSON = genericToJSON jsonOptions

instance FromJSON ReqCurrencies where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqCurrencies where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResCurrencies where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResCurrencies where
    toJSON = genericToJSON jsonOptions

instance FromJSON ReqDeposit where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqDeposit where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResDeposit where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResDeposit where
    toJSON = genericToJSON jsonOptions

instance FromJSON ReqExtract where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqExtract where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResExtract where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResExtract where
    toJSON = genericToJSON jsonOptions

instance FromJSON Entry where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON Entry where
    toJSON = genericToJSON jsonOptions
instance FromJSON ReqInspectLedger where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqInspectLedger where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResInspectLedger where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResInspectLedger where
    toJSON = genericToJSON jsonOptions
    
instance FromJSON ReqSendMoney where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqSendMoney where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResSendMoney where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResSendMoney where
    toJSON = genericToJSON jsonOptions

instance FromJSON ReqUsers where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqUsers where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResUsers where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResUsers where
    toJSON = genericToJSON jsonOptions

instance FromJSON ReqUserBalances where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ReqUserBalances where
    toJSON = genericToJSON jsonOptions
instance FromJSON ResUserBalances where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON ResUserBalances where
    toJSON = genericToJSON jsonOptions
