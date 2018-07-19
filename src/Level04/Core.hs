{-# LANGUAGE OverloadedStrings #-}
module Level04.Core
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Conf                       (Conf, firstAppConfig, dbFilePath)
import qualified Level04.DB                         as DB
import           Level04.DB                         (initDB)
import           Level04.Types                      (ContentType (JSON, PlainText),
                                                     Error (EmptyCommentText, EmptyTopic, UnknownRoute, ConversionError),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  db <- prepareAppReqs
  case db of
    Right db_ -> run 3000 (app db_)
    Left _ -> pure ()

-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
--
-- Our application configuration is defined in Conf.hs
--
prepareAppReqs
  :: IO ( Either StartUpError DB.FirstAppDB )
prepareAppReqs = 
  do
    db <- initDB $ dbFilePath firstAppConfig
    case db of
      Left e -> pure $ Left $ DbInitErr e
      Right fdb -> pure $ Right fdb
  -- error "prepareAppReqs not implemented"

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: ToJSON a
  => a
  -> Response
resp200Json =
  mkResponse status200 JSON . A.encode

-- |
app
  :: DB.FirstAppDB -- ^ Add the Database record to our app so we can use it
  -> Application
app db rq cb = do
  rq' <- mkRequest rq
  resp <- handleRespErr <$> handleRErr rq'
  cb resp
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

    -- We want to pass the Database through to the handleRequest so it's
    -- available to all of our handlers.
    handleRErr :: Either Error RqType -> IO (Either Error Response)
    handleRErr = either ( pure . Left ) ( handleRequest db )

handleRequest
  :: DB.FirstAppDB
  -> RqType
  -> IO (Either Error Response)
handleRequest _db (AddRq t c) =
  (resp200 PlainText "Success" <$) <$> DB.addCommentToTopic _db t c
  -- error "AddRq handler not implemented"

  -- what is <$, and what does it do?

  -- :t (<$)
  -- (<$) :: Functor f => a -> f b -> f a

  -- it takes a value 'a', and a functor with 'b',
  --    it throws 'b' away, and reuses the functor
  -- for example:
  -- 1 <$ Right 'c'
  -- Right 1
  
  -- similarly to const, it keeps left value
  -- const 1 (Right 'c')
  -- 1

  -- if we add fmap:
  -- (fmap . const) 1 (Right 'c')
  -- Right 1
  -- so (<$) == (fmap . const)

  -- interestingly:
  -- 1 <$ (Left 'c')
  -- Left 'c'
  -- it picked 'c' instead of 1!
  -- it also picked up the Either functor

  -- so now what's all this (<$) together with <$> business then?
  -- this is needed for nested functors
  -- fmap lifts a function onto a functor

  -- (1 <$) (Right 3)
  -- Right 1
  -- that works. what about:
  -- (1 <$) (Right (Right 3))
  -- Right 1
  -- what if we wanted (Right (Right 1)) instead?

  -- fmap!
  -- (1 <$) <$> (Right (Right 3))
  -- Right (Right 1)

handleRequest _db (ViewRq t)  =
  (fmap . fmap) resp200Json (DB.getComments _db t)
handleRequest _db ListRq      =
  (fmap . fmap) resp200Json (DB.getTopics _db)

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse ConversionError =
  resp400 PlainText "Converison Error"
