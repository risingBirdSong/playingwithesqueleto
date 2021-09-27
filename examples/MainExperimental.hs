{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}


module MainExperimental
    ( main
    ) where

import Blog
import Control.Monad (void)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Monoid ((<>))
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH
       ( AtLeastOneUniqueKey(..)
       , OnlyOneUniqueKey(..)
       , mkDeleteCascade
       , mkMigrate
       , mkPersist
       , persistLowerCase
       , share
       , sqlSettings
       )
import qualified UnliftIO.Resource as R

import qualified Data.Text as T
import qualified Data.Text.IO as T 
import Data.Text.Encoding         as T


import qualified Control.Monad.Trans.Reader as RD 
import Database.Esqueleto.Internal.Internal (SqlSelect)
import Control.Monad.Logger
import Debug.Trace





-- createuser myesq --pwprompt --superuser
-- createdb myesq
share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name String
    age Int Maybe
    deriving Eq Show
  BlogPost
    title String
    authorId PersonId
    deriving Eq Show
  Follow
    follower PersonId
    followed PersonId
    deriving Eq Show
|]

setupDb :: (MonadIO m, MonadLogger m)
          => SqlPersistT m ()
setupDb = do
  -- | Run migrations and create the test database entries
  runMigration migrateAll
  createDb
  where
    createDb :: (MonadIO m, MonadLogger m)
           => SqlPersistT m ()
    createDb = do
      john <- insert $ Person "John" (Just 24)
      sean <- insert $ Person "Seán" (Just 70)
      joao <- insert $ Person "Joao" (Just 13)
      void $ insertMany [ BlogPost "How to play a bodhrán" sean
                        , BlogPost "Haskell for the working class hero" john
                        ]
      void $ insert $ Follow john sean
      void $ insert $ Follow sean john
      void $ insert $ Follow joao sean


cleanDb :: (MonadIO m, MonadLogger m)
        => SqlPersistT m ()
cleanDb = do
  -- | Drop the tables so we can re-run the script again if needed
  dropTable "follow"
  dropTable "blog_post"
  dropTable "person"
  where
    dropTable tableName = rawExecute ("DROP TABLE " <> tableName) []

main :: IO ()
main = pure ()
-- main = do
--   -- Connection string for the postrgesql database
--   runBlogT connection . runDB $ do
--     setupDb
--     say "johns"
--     johns <- getJohns
--     mapM_ say johns
--     say "adults"
--     adults <- getAdults
--     mapM_ say adults

--     -- cleanDb
--   where
--     say :: (MonadIO m, Show a) => a -> m ()
--     say = liftIO . print
--     connection = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"

runDB :: (MonadReader ConnectionString m,
          MonadIO m,
          MonadBaseControl IO m,
          MonadUnliftIO m,
          MonadLoggerIO m,
          MonadLogger m)
      => SqlPersistT m a -> m a
runDB query = do
  -- | Helper for running a query
  conn <- ask
  withPostgresqlConn conn $ \backend -> runReaderT query backend
-- "host=localhost port=5432 user=myesq password=myesq dbname=myesq"
easyRender query backend =
    let 
        whole = 
            runReaderT (renderQuerySelect query) backend
    in
        (uncurry spliceValues) <$> whole

easyRender' :: (SqlSelect a r, BackendCompatible SqlBackend backend, Monad m) => SqlQuery a -> RD.ReaderT backend m T.Text
easyRender' query = do
  whole <- renderQuerySelect query 
  pure $ uncurry spliceValues whole

myInsert :: IO ()
myInsert = do 
  runBlogT connection . runDB $ do
    setupDb
    say $ "setting up db" 
  where
    say :: (MonadIO m, Show a) => a -> m ()
    say = liftIO . print
    connection = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"

myTearDown :: IO ()
myTearDown = do 
  runBlogT connection . runDB $ do
    cleanDb
    say $ "tearing down db" 
  where
    say :: (MonadIO m, Show a) => a -> m ()
    say = liftIO . print
    connection = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"


-- callEasyRender :: (MonadReader ConnectionString m,
--           MonadIO m,
--           MonadBaseControl IO m,
--           MonadUnliftIO m,
--           MonadLoggerIO m,
--           MonadLogger m) => m T.Text
callEasyRender qry = do
  --  void $ runBlogT connection . runDB $ do
   splitquery@(txt, vals) <- runDB $ renderQuerySelect qry
   liftIO $ print "show qry txt"
   liftIO $ print txt

   liftIO $ putStrLn "qry text"
   liftIO $ T.putStrLn txt
   liftIO $ putStrLn "qry vals"
   liftIO $ print vals
   unifiedQuery <- runDB $  easyRender' qry
   pure unifiedQuery
    where 
      connection = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"

myMain = do
  let conn = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"
  -- resultA <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleA) conn
  -- print "resultA"
  -- T.putStrLn resultA
  -- print "..."
  -- resultB <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleB) conn
  -- print "resultB"
  -- print "..."
  -- T.putStrLn resultB
  -- resultC <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleC) conn
  -- print "resultC"
  -- print "..."
  -- T.putStrLn resultC

  resultD <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleD) conn
  print "resulD"
  print "..."
  T.putStrLn resultD

  -- resultE <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleE) conn
  -- print "resultE"
  -- print "..."
  -- T.putStrLn resultE

  -- resultF <- runStderrLoggingT $ runReaderT (callEasyRender $ queryExampleF (Just (10 :: Int))) conn
  -- print "resultF"
  -- print "..."
  -- T.putStrLn resultF

  -- resultG <- runStderrLoggingT $ runReaderT (callEasyRender $ queryExampleG) conn
  -- print "resultG"
  -- print "..."
  -- T.putStrLn resultG


queryExampleA = do  
                people <- from $ table @Person
                where_ (people ^. PersonName ==. val "John")
                pure people


queryExampleB = do  
                people <- from $ table @Person
                where_ (people ^. PersonAge >=. just (val 18))
                pure people


queryExampleCText = T.pack "SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ?) AND (\"person\".\"age\" >= ?)\n"


queryExampleC = do  
                people <- from $ table @Person
                where_ (people ^. PersonName ==. val "John")
                where_ (people ^. PersonAge >=. just (val 18))
                pure people

queryExampleD = do 
                people <- from $ table @Person
                where_ (people ^. PersonName ==. val "John")
                where_ (people ^. PersonAge >=. just (val 18))
                where_ (people ^. PersonAge <=. just (val 99))
                pure people

queryExampleE = do 
                people <- from $ table @Person
                where_ (people ^. PersonName ==. val "John")
                where_ (people ^. PersonAge >=. just (val 18))
                where_ (people ^. PersonAge <=. just (val 99))
                where_ (people ^. PersonAge ==. just (val 50))
                pure people


-- modified from the example docs 
queryExampleF age = do
  (people :& blogPosts) <-
      from $ table @Person
      `InnerJoin` table @BlogPost
      `on` (\(people :& blogPosts) ->
              people ^. PersonId ==. blogPosts ^. BlogPostAuthorId)
  where_ (people ^. PersonAge >. val age)
  pure (people, blogPosts)

queryExampleG = do
  (people1 :& followers :& people2) <-
      from $ table @Person
      `innerJoin` table @Follow
      `on` (\(people1 :& followers) ->
              people1 ^. PersonId ==. followers ^. FollowFollowed)
      `innerJoin` table @Person
      `on` (\(_ :& followers :& people2) ->
              followers ^. FollowFollower ==. people2 ^. PersonId)
  where_ (people1 ^. PersonName ==. val "John")
  pure (followers, people2)


spliceValues :: T.Text -> [PersistValue] -> T.Text
spliceValues query values = 
    let
        -- removed init
        fragments = (T.splitOn "?" query)
        seefragments = (T.splitOn "?" query)
    in
        
       trace (show fragments) (combineTexts fragments (map valueToText values))-- <> T.replicate (length fragments -1 ) (T.pack ")")
      -- the replicate handles this situation
      -- case (length fragments) of)
      --   1 -> (combineTexts fragments (map valueToText values))
      --   2 -> (combineTexts fragments (map valueToText values)) <> (T.pack ")")
      --   3 -> (combineTexts fragments (map valueToText values)) <> (T.pack ")") <> (T.pack ")")
      --   4 -> (combineTexts fragments (map valueToText values)) <> (T.pack ")") <> (T.pack ")") <> (T.pack ")")

-- ourexample = ["SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = "
-- ,") AND ((\"person\".\"age\" >= "
-- ,") AND (\"person\".\"age\" <= ","))\n"]

combineTexts :: [T.Text] -> [T.Text] -> T.Text
-- combineTexts [] [] = mempty 
combineTexts [singleton] [] =  singleton
combineTexts (r:rawsql) (v:vals) =  (r <> v) <> (combineTexts rawsql vals)
combineTexts raw vals = error $ "we expected two lists of equal length raw -> " ++ (show raw) ++ " vals -> " ++ (show vals) 


valueToText :: PersistValue -> T.Text
valueToText input = case input of
    (PersistText txt) -> (T.singleton '\'') <> txt <> (T.singleton '\'')
    (PersistInt64 int64) -> T.pack (show int64)
    (PersistByteString btstr) -> T.decodeUtf8 btstr
    (PersistDouble dbl) -> T.pack (show dbl)
    (PersistDay day) -> T.pack $ show day	
    (PersistTimeOfDay timeofday) -> T.pack $ show timeofday
    (PersistUTCTime utctime) -> T.pack $ show utctime 
    _ -> error "unsupported"	






















aa = T.splitOn ("?") queryExampleCText 
bb = ["SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ",") AND (\"person\".\"age\" >= ",")\n"]


rawsqlA =  "SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ?) AND (\"person\".\"age\" >= ?)\n"

splittingquestion = T.splitOn (T.pack "?") (T.pack rawsqlA)

splitOnSpaces = T.splitOn (T.pack " ") (T.pack afterJoin)
splitted = ["SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ",") AND (\"person\".\"age\" >= "]


variables = [PersistText "John",PersistInt64 18]
variablesAsText = [T.pack ("John"),(T.pack "18")]

afterJoin = "SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = John) AND (\"person\".\"age\" >= 18"


aaa = init $ T.splitOn "?" ("select * from ?")

