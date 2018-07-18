{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types           (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType, mkCommentText, getCommentText, getTopic)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType = responseLBS status [("Content-Type", renderContentType contentType)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest "" _ = Left EmptyRequest
mkAddRequest topic commentText = do
  text <- mkCommentText $ lazyByteStringToStrictText commentText
  t <- mkTopic topic
  Right (AddRq t text)

  -- mkCommentText (lazyByteStringToStrictText commentText) >>= \c ->
  --   mkTopic topic >>= \t ->
  --     Right (AddRq t c)

  -- case mkCommentText $ lazyByteStringToStrictText commentText of
  -- Right text -> case mkTopic topic of
  --   Right t -> Right (AddRq t text)
  --   Left e -> Left e
  -- Left e -> Left e
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic = do
  t <- mkTopic topic 
  Right (ViewRq t)

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

mkUnknownRequest
  :: Either Error a
mkUnknownRequest = Left UnknownRequest

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic = resp400 Text "Error: topic not provided"
mkErrorResponse EmptyComment = resp400 Text "Error: comment not provided"
mkErrorResponse EmptyRequest = resp400 Text "Error: request not provided"
mkErrorResponse UnknownRequest = resp404 Text "Error: unknown request"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r = case (pathInfo r, requestMethod r) of
  (["list"], "GET") -> pure mkListRequest
  ([topic, "view"], "GET") -> pure (mkViewRequest topic)
  ([topic, "add"], "POST") -> mkAddRequest topic <$> strictRequestBody r
  _ -> pure mkUnknownRequest
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest ListRq = Right $ resp200 Text "List method is not implemented"
handleRequest (ViewRq _) = Right $ resp200 Text "Viewed"
handleRequest (AddRq _ _) = Right $ resp200 Text "Added"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app req callback =
  -- (mkRequest req) >>= return . (\x -> _) . (>>= handleRequest) >>= callback

  do
    thing <- mkRequest $ req
    callback $ either mkErrorResponse id (thing >>= handleRequest)

  --   do
  -- thing <- mkRequest $ req 
  -- callback $ case thing of
  --   Left e -> mkErrorResponse e
  --   Right a -> either mkErrorResponse id (handleRequest a)


  --   do
  -- thing <- mkRequest $ req 
  -- callback $ case thing of
  --   Left e -> mkErrorResponse e
  --   Right a -> case handleRequest a of
  --     Right a' -> a'
  --     Left e' -> mkErrorResponse e'

runApp :: IO ()
runApp = run 3000 app
