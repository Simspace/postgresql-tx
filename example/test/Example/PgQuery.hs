module Example.PgQuery
  ( DB.Handle(..)
  , Queries.new
  , Queries.withHandle
  ) where

import qualified Example.PgQuery.Internal.DB as DB
import qualified Example.PgQuery.Internal.Queries as Queries
