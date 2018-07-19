{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text, pack)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment, CommentText,
                                                     Error, Topic, getTopic, mkTopic,
                                                     fromDbComment, getCommentText)
import           Level04.DB.Types                   (DBComment)
-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB db = Sql.close $ dbConn db

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp =
  do
  conn <- Sql.open fp
  Sql.runDBAction $ Sql.execute_ conn createTableQ
  pure $ Right $ FirstAppDB conn
  -- error "initDb not implemented"
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments fdb topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
    -- thing = Sql.fold _ _ _ _
  in
    traverse fromDbComment <$> Sql.query (dbConn fdb) sql [getTopic topic]

      -- do
      --   q <- Sql.query (dbConn fdb) sql [getTopic topic] -- :: IO [DBComment]
      --   pure $ traverse fromDbComment q
    -- error "getComments not implemented"

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic fdb topic commentText =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in
    do
      t <- getCurrentTime
      thing <- Sql.execute (dbConn fdb) sql (getTopic topic, getCommentText commentText, t)
      pure . Right $ thing

    -- getCurrentTime >>= \x -> Sql.execute (dbConn fdb) sql [getTopic topic, getCommentText commentText, pack $ show x] >>= pure . Right

    -- error "addCommentToTopic not implemented"

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics fdb =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in
    traverse (mkTopic . Sql.fromOnly) <$> (Sql.query_ (dbConn fdb) sql :: IO [Sql.Only Text])
    -- error "getTopics not implemented"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic fdb topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in
    Right <$> Sql.execute (dbConn fdb) sql (Sql.Only . getTopic $ topic)
    -- error "deleteTopic not implemented"
