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


getJohns :: (MonadIO m, MonadLogger m)
         => SqlReadT m [Entity Person]
getJohns =  select $ do  
                people <- from $ table @Person
                where_ (people ^. PersonName ==. val "John")
                pure people

getJohnsRender :: (MonadIO m, MonadLogger m)
         => SqlReadT m (T.Text, [PersistValue])
getJohnsRender =  myRenderQuerySelect $ do  
                    people <- from $ table @Person
                    where_ (people ^. PersonName ==. val "John")
                    pure people

getAdults :: (MonadIO m, MonadLogger m)
          => SqlReadT m [Entity Person]
getAdults =
  -- | Select any Person where their age is >= 18 and NOT NULL
  select $ do
    people <- from $ table @Person
    where_ (people ^. PersonAge >=. just (val 18))
    -- return p
    pure (people)


renderGetJohnsAndAdults :: (MonadIO m, MonadLogger m) => SqlReadT m (T.Text, [PersistValue])
renderGetJohnsAndAdults =  myRenderQuerySelect $ do  
                    people <- from $ table @Person
                    where_ (people ^. PersonName ==. val "John")
                    where_ (people ^. PersonAge >=. just (val 18))
                    pure people



-- getBlogPostsByAuthors :: (MonadIO m, MonadLogger m)
--                       => SqlReadT m [(Entity BlogPost, Entity Person)]
-- getBlogPostsByAuthors =
--   -- | Select all persons and their blogposts, ordering by title
--   select $
--   from $ \(b, p) -> do
--     where_ (b ^. BlogPostAuthorId ==. p ^. PersonId)
--     orderBy [asc (b ^. BlogPostTitle)]
--     return (b, p)


-- getAuthorMaybePosts :: (MonadIO m, MonadLogger m)
--                     => SqlReadT m [(Entity Person, Maybe (Entity BlogPost))]
-- getAuthorMaybePosts =
--   -- | Select all persons doing a left outer join on blogposts
--   -- | Since a person may not have any blogposts the BlogPost Entity is wrapped
--   -- | in a Maybe
--   select $
--   from $ \(p `LeftOuterJoin` mb) -> do
--     on (just (p ^. PersonId) ==. mb ?. BlogPostAuthorId)
--     orderBy [asc (p ^. PersonName), asc (mb ?. BlogPostTitle)]
--     return (p, mb)


-- followers :: (MonadIO m, MonadLogger m)
--           => SqlReadT m [(Entity Person, Entity Follow, Entity Person)]
-- followers =
--   -- | Select mutual follow relationships
--   -- | Note carefully that the order of the ON clauses is reversed!
--   -- | You're required to write your ons in reverse order because that helps composability
--   -- | (see the documentation of on for more details).
--   select $
--   from $ \(p1 `InnerJoin` f `InnerJoin` p2) -> do
--     on (p2 ^. PersonId ==. f ^. FollowFollowed)
--     on (p1 ^. PersonId ==. f ^. FollowFollower)
--     return (p1, f, p2)


-- updateJoao :: (MonadIO m, MonadLogger m)
--            => SqlWriteT m ()
-- updateJoao =
--   -- Update the name of any Joao in our person table to João
--   update $ \p -> do
--     set p [ PersonName =. val "João" ]
--     where_ (p ^. PersonName ==. val "Joao")


-- deleteYoungsters :: (MonadIO m, MonadLogger m)
--                  => SqlPersistT m ()
-- deleteYoungsters = do
--   -- | Delete any persons under the age of 14

--   -- | In this case where `ON DELETE CASCADE` is not generated by migration
--   -- | we select all the entities we want to delete and then for each one
--   -- | one we extract the key and use Persistent's `deleteCascade`
--   youngsters <- select $
--     from $ \p -> do
--     where_ (p ^. PersonAge <. just (val 14))
--     pure p
--   forM_ youngsters (deleteCascade . entityKey)


-- insertBlogPosts :: (MonadIO m, MonadLogger m)
--                 => SqlWriteT m ()
-- insertBlogPosts =
--   -- | Insert a new blogpost for every person
--   insertSelect $ from $ \p ->
--     return $ BlogPost <# (val "Group Blog Post") <&> (p ^. PersonId)
-- withConn :: ConnectionString
-- withConn =
--   R.runResourceT . withPostgresqlConn "host=localhost port=5432 user=esqutest password=esqutest dbname=esqutest"

-- let myrender = myRenderQuerySelect 



-- myRunDB :: (MonadIO m,
--             -- MonadBaseControl IO m,
--             -- MonadUnliftIO m,
--             MonadLoggerIO m,
--             MonadLogger m)
--         => SqlPersistT m a -> m a
-- myRunDB query = withPostgresqlConn "host=localhost port=5432 user=myesq password=myesq dbname=myesq" $ \backend -> runReaderT query backend


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
main = do
  -- Connection string for the postrgesql database
  runBlogT connection . runDB $ do
    setupDb
    say "johns"
    johns <- getJohns
    mapM_ say johns
    say "adults"
    adults <- getAdults
    mapM_ say adults

    cleanDb
  where
    say :: (MonadIO m, Show a) => a -> m ()
    say = liftIO . print
    connection = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"

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
    -- let 
    --     whole = renderQuerySelect query         
    -- in
    --     (uncurry spliceValues) <$> whole

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
   ourquerystring <- runDB $  easyRender' qry
   pure ourquerystring
    where 
      connection = "host=localhost port=5432 user=myesq dbname=myesq password=myesq"

myMain = do
   resultA <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleA) "host=localhost port=5432 user=myesq dbname=myesq password=myesq"
   print "resultA"
   T.putStrLn resultA
   print "..."
   resultB <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleB) "host=localhost port=5432 user=myesq dbname=myesq password=myesq"
   print "resultB"
   print "..."
   T.putStrLn resultB
   resultC <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleC) "host=localhost port=5432 user=myesq dbname=myesq password=myesq"
   print "resultC"
   print "..."
   T.putStrLn resultC

   resultD <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleD) "host=localhost port=5432 user=myesq dbname=myesq password=myesq"
   print "resultD"
   print "..."
   T.putStrLn resultD

   resultE <- runStderrLoggingT $ runReaderT (callEasyRender queryExampleE) "host=localhost port=5432 user=myesq dbname=myesq password=myesq"
   print "resultE"
   print "..."
   T.putStrLn resultE
  

-- renderQuerySelect
--   :: (SqlSelect a r, BackendCompatible SqlBackend backend,
--       Monad m) =>
--      SqlQuery a
--      -> Control.Monad.Trans.Reader.ReaderT
--           backend m (Data.Text.Internal.Text, [PersistValue])


getJohnsAndAdults :: (MonadIO m, MonadLogger m) => SqlReadT m [Entity Person]
getJohnsAndAdults = select $ do  
                    people <- from $ table @Person
                    where_ (people ^. PersonName ==. val "John")
                    where_ (people ^. PersonAge >=. just (val 18))
                    pure people


-- getJohns :: (MonadIO m, MonadLogger m)
--          => SqlReadT m [Entity Person]
-- getJohns =  select $ do  
--                 people <- from $ table @Person
--                 where_ (people ^. PersonName ==. val "John")
--                 pure people

queryExampleA = do  
                people <- from $ table @Person
                where_ (people ^. PersonName ==. val "John")
                pure people


queryExampleB = do  
                people <- from $ table @Person
                where_ (people ^. PersonAge >=. just (val 18))
                pure people


queryExampleCText = T.pack "SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ?) AND (\"person\".\"age\" >= ?)\n"

aa = T.splitOn ("?") queryExampleCText 
bb = ["SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ",") AND (\"person\".\"age\" >= ",")\n"]
-- cc = T.splitOn ("\n") bb
-- fdf = 1

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


spliceValues :: T.Text -> [PersistValue] -> T.Text
spliceValues query values = 
    let
        fragments = init $ (T.splitOn "?" query)
        seefragments = (T.splitOn "?" query)
    in
        (combineTexts fragments (map valueToText values)) <> T.replicate (length fragments -1 ) (T.pack ")")
      -- the replicate handles this situation
      -- case (length fragments) of)
      --   1 -> (combineTexts fragments (map valueToText values))
      --   2 -> (combineTexts fragments (map valueToText values)) <> (T.pack ")")
      --   3 -> (combineTexts fragments (map valueToText values)) <> (T.pack ")") <> (T.pack ")")
      --   4 -> (combineTexts fragments (map valueToText values)) <> (T.pack ")") <> (T.pack ")") <> (T.pack ")")


combineTexts :: [T.Text] -> [T.Text] -> T.Text
combineTexts [] [] = mempty 
combineTexts (r:rawsql) (v:vals) =  (r <> v) <> (combineTexts rawsql vals) 
combineTexts _ _ = error "we expected two lists of equal length"


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
    -- missing a whole lot of cases, TODO



    -- (PersistInt64 int64) -> T.pack (show int64) 

convertInt64 :: PersistValue -> T.Text
convertInt64 (PersistInt64 int64) = T.pack (show int64)  
--                       = s


rawsqlA =  "SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ?) AND (\"person\".\"age\" >= ?)\n"

splittingquestion = T.splitOn (T.pack "?") (T.pack rawsqlA)

splitOnSpaces = T.splitOn (T.pack " ") (T.pack afterJoin)
a = 1
-- [PersistText "John",PersistInt64 18]



splitted = ["SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = ",") AND (\"person\".\"age\" >= "]


variables = [PersistText "John",PersistInt64 18]
variablesAsText = [T.pack ("John"),(T.pack "18")]

afterJoin = "SELECT \"person\".\"id\", \"person\".\"name\", \"person\".\"age\"\nFROM \"person\"\nWHERE (\"person\".\"name\" = John) AND (\"person\".\"age\" >= 18"


aaa = init $ T.splitOn "?" ("select * from ?")

-- todo make a new function cleanText that will work with text, for now I just want to keep moving with this
-- https://stackoverflow.com/questions/3740621/removing-string-double-quotes-in-haskell/3743602
cleanString :: String -> String
cleanString s@[c]                     = s
cleanString ('"':s)  
            | last s == '"'  = init s
            | otherwise      = s
cleanString ('\'':s) 
            | last s == '\'' = init s
            | otherwise      = s
cleanString s   = s