{-# LANGUAGE QuasiQuotes #-}
module SQL where

import Hasql.TH
import Hasql.Session (Session, sql)

initSQL :: Session ()
initSQL = sql [uncheckedSql|
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS games (
  id uuid NOT NULL PRIMARY KEY,
  max_users int NOT NULL,
);
|]
