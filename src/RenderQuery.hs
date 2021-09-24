{-# LANGUAGE OverloadedStrings #-}

module RenderQuery where

import Data.Text
-- renderQuerySelect :: SqlQuery a -> SqlPersistT m (Text, [PersistValue])

-- easyRender :: SqlQuery a -> Text
-- easyRender query =
--     let 
--         (text, persistValues) = 
--             runReaderT (renderQuerySelect query) metabaseBackend
--     in
--         spliceValues text persistValues

-- | This version does not work if the SQL query has a question mark
-- already, inside of a string literal.
--
-- > SELECT *
-- > FROM users AS u
-- > WHERE u.name = 'Hello?'
--

-- spliceValues :: Text -> [PersistValue] -> Text
-- spliceValues query values = 
--     let
--         fragments = 
--             Text.splitOn "?" query
--     in
--         combineTexts fragments (map valueToText values)

-- | Kind of a zip, where we take an element from the first list, then an
-- element from the second list, and cycle that

-- combineTexts :: [Text] -> [Text] -> Text
-- combineTexts [] [] = mempty 
-- combineTexts (r:rawsql) (v:vals) =  (r <> v) <> (combineTexts rawsql vals) 
-- combineTexts _ _ = error "we expected two lists of equal length"

-- example = "afasdf ? adsfadsf ? dfasdf ? dfdad ?"

-- splitted = splitOn  "?" (pack example)

-- render a PersistValue into something that a query can use
-- directly
-- valueToText :: PersistValue -> Text
-- valueToText input = case input of
--     (PersistText "  content  ) -> " \' content ' 
--     (PersistText content  ) -> "content " 
    
    

--     PersistText Text	 
-- PersistByteString ByteString	 
-- PersistInt64 Int64	 
-- PersistDouble Double	 
-- PersistRational Rational	 
-- PersistBool Bool	 
-- PersistDay Day	 
-- PersistTimeOfDay TimeOfDay	 
-- PersistUTCTime UTCTime	 
-- PersistNull	 
-- PersistList [PersistValue]	 
-- PersistMap [(Text, PersistValue)]	 
-- PersistObjectId ByteString	
-- Intended especially for MongoDB backend

-- PersistArray [PersistValue]	
-- Intended especially for PostgreSQL backend for text arrays

-- PersistLiteral_ LiteralType ByteString


-- support ticket 1528983483