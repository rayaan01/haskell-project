module Database where

import Control.Applicative
import Database.SQLite.Simple                   
import Database.SQLite.Simple.FromRow           
import Data.Time  

data Tool = Tool
 { toolId :: Int
 , name :: String
 , description :: String
 , lastReturned :: Day
 , timesBorrowed :: Int
 }

 data User = User
 { userId :: Int
 , userName :: String
 }

 instance Show User where
   show user = mconcat [ show $ userId user
                       , ".)  "
                       , userName user]

instance Show Tool where
   show tool = mconcat [ show $ toolId tool
                       , ".) "
                       , name tool
                       , "\n description: "
                       , description tool
                       , "\n last returned: "
                       , show $ lastReturned tool
                       , "\n times borrowed: "
                       , show $ timesBorrowed tool
                       , "\n"]

addUser :: String -> IO ()
addUser userName = do
   conn <- open "tools.db"                                      
   execute conn "INSERT INTO users (username) VALUES (?)"       
     (Only userName)                                            
     print "user added"
   close conn  


withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn


checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                         \conn -> do
                           execute conn
                             "INSERT INTO checkedout
                             (user_id,tool_id) VALUES (?,?)"
                             (userId,toolId)