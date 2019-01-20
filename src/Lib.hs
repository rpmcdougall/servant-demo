{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.List
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
    let maybeUser = findUserById n
    case maybeUser of
      Just user -> return user
      Nothing -> Handler $ (throwE $ err404 { errBody = "Could not find user with that ID"})


findUserById :: Int -> Maybe User
findUserById n = find (\(User userId _ _) -> userId == n) users

type UsersApi = "users" :> Get '[JSON] [User]
  :<|> "user" :> Capture "userid" Int :> Get '[JSON] User


usersServer :: Server UsersApi
usersServer = getUsers
         :<|> getUserById

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

usersAPI :: Proxy UsersApi
usersAPI = Proxy :: Proxy UsersApi

runServer :: IO ()
runServer = do
  putStrLn "Starting API on port: 8000"
  run 8000 (serve usersAPI (usersServer))