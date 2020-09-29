{-|
Module    : Database.Orville.PostgreSQL.Raw
Copyright : Flipstone Technology Partners 2020
License   : MIT
-}

module Database.Orville.PostgreSQL
  ( createConnectionPool
  , SqlType ( SqlType
            , sqlTypeDDL
            , sqlTypeReferenceDDL
            , sqlTypeNullable
            , sqlTypeId
            , sqlTypeSqlSize
            , sqlTypeToSql
            , sqlTypeFromSql
            )
  , integer
  )
where

import Database.Orville.PostgreSQL.Connection (createConnectionPool)
import Database.Orville.PostgreSQL.Internal.SqlType (SqlType ( SqlType
                                                             , sqlTypeDDL
                                                             , sqlTypeReferenceDDL
                                                             , sqlTypeNullable
                                                             , sqlTypeId
                                                             , sqlTypeSqlSize
                                                             , sqlTypeToSql
                                                             , sqlTypeFromSql
                                                             )
                                                    , integer
                                                    )
