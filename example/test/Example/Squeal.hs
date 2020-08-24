module Example.Squeal
  ( DB.Handle(..)
  , Queries.new
  , Queries.withHandle
  , Schema.Schemas
  ) where

import qualified Example.Squeal.Internal.DB as DB
import qualified Example.Squeal.Internal.Queries as Queries
import qualified Example.Squeal.Internal.Schema as Schema
