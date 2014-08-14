
module Main ( main )
 where

import System.Environment ( getArgs )
import System.Directory ( getHomeDirectory )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Database.HDBC ( quickQuery, toSql, disconnect, commit, runRaw, fromSql, SqlValue)
import Database.HDBC.Sqlite3 ( Connection, connectSqlite3 )

main = fmap parse getArgs >>= 
       doAction 

newtype Label = Label String
            deriving(Show)

newtype Snippet = Snippet String
            deriving(Show)

data SnippetAction = Usage
                    | Lookup Label
                    | Add Label Snippet
                    deriving(Show)

parse :: [String] -> SnippetAction
parse ["-h"] = Usage
parse []     = Usage
parse [x]    = Lookup $ Label x
parse (label:snippet:_) = Add (Label label) (Snippet snippet)

doAction :: SnippetAction -> IO() 
doAction Usage = putStrLn "Usage: snpt [-h] label [snippet]\n"
doAction (Lookup (Label l)) = dbConnection >>=
                              (\conn -> quickQuery conn selectSQL [ toSql (l ++ "%") ]) >>=
                              \result -> mapM_ (\(l:s:[]) -> print $ UnquotedString $ sqlToString l ++ " := " ++ sqlToString s) result

doAction (Add (Label l) (Snippet s)) = dbConnection >>=
                                       \conn ->  getPOSIXTime >>=
                                       \time -> quickQuery conn insertSQL [ toSql l, toSql s, toSql (round time :: Int)] >>
                                       commit conn >>
                                       print ( UnquotedString "Snippet added" )

-- define type to suppress quotes
-- http://stackoverflow.com/questions/12102874/haskell-suppress-quotes-around-strings-when-shown
newtype UnquotedString = UnquotedString String
instance Show UnquotedString where
    show (UnquotedString s) = s


sqlToString :: SqlValue -> String
sqlToString = fromSql

resultToString :: [[SqlValue]] -> [[String]]
resultToString = map (map sqlToString) 

-- Get a connection to the db. Sqlite creates the db if it doest exist 
dbConnection :: IO Connection 
dbConnection = fmap (++ "/.snpt.db") getHomeDirectory >>=
               connectSqlite3 >>=
               \conn -> runRaw conn sqlDDL >> 
               commit conn >>
               return conn

sqlDDL::String
sqlDDL = "CREATE TABLE IF NOT EXISTS snippets ( label VARCHAR(255) PRIMARY KEY ON CONFLICT REPLACE, snippet TEXT, create_date INT NOT NULL)"

selectSQL::String
selectSQL = "SELECT label, snippet FROM snippets WHERE label like ?"

insertSQL::String
insertSQL = "INSERT INTO snippets(label, snippet, create_date) VALUES( ?,?,? )"


