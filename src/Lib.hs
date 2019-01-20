{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.List
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


getUsers :: Handler [User]
getUsers = return users

getUserById :: Int -> Handler User
getUserById n = do 
    maybeUser <- findUserById n
    case maybeUser of
      Just user -> return user
      Nothing -> Handler $ (throwE $ err404 { errBody = "Could not find user with that ID" })


findUserById :: Int -> Maybe User
findUserById n = find (\(User userId _ _) -> userId == n) users

type API = "users" :> Get '[JSON] [User]
  :<|> "user" :> Capture "userid" Integer :> Get '[JSON] User

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return getUserById 
         :<|> getUsers

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]




