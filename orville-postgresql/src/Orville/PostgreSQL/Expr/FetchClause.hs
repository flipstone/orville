{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.FetchClause
  ( FetchClause
  , fetchClauseInt
  , FetchClauseModifier
  , withTiesFetchClauseModifier
  , onlyFetchClauseModifier
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | Type to represent a SQL FETCH clause. E.G.

> FETCH FIRST 2 ROWS ONLY

'FetchClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FetchClause
  = FetchClause RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Build a SQL FETCH clause for a given number of rows and 'FetchClauseModifier'.

@since 1.1.0.0
-}
fetchClauseInt :: Int -> FetchClauseModifier -> FetchClause
fetchClauseInt fetchNumber =
  internal_fetchRaw (RawSql.parameter (SqlValue.fromInt fetchNumber))

-- Internal helper for FETCH. Note this allows illegal expressions to be put together. However many
-- potentially interesting expressions are valid. Such as:
--
-- `VALUES (1, 'o'), (2, 't'), (3, 'th') order by column2 fetch first (select 2) ROW with ties;`
--
-- How to best expose these is not clear at this moment as we do not generally allow any raw sql to
-- be injected, but we do not have a good expression to capture the allowed values here. Finally, it
-- would _appear_ that the same restrictions here would also apply to expressions allowed for
-- LIMIT. Which as of this writing, we do not have a helper for creation.
--
-- Finally note that NEXT and ROW are as PostgreSQL docs call them, noise words, they do not impact
-- the meaning. See https://www.postgresql.org/docs/13/sql-select.html. 'NEXT' and 'ROW' are chosen
-- over 'FIRST' and 'ROWS' respecitvely because they are shorter and thus result in a smaller
-- bytestring being sent over the wire.
internal_fetchRaw :: RawSql.RawSql -> FetchClauseModifier -> FetchClause
internal_fetchRaw countingRawSql modifier =
  FetchClause $
    RawSql.fromString "FETCH NEXT "
      <> RawSql.parenthesized countingRawSql
      <> RawSql.fromString " ROW "
      <> RawSql.toRawSql modifier

{- | Type to represent the treatment of ties in a SQL FETCH expression. E.G.

> WITH TIES

'FetchClauseModifier' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FetchClauseModifier
  = FetchClauseModifier RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Allow a FETCH clause to have ties.

@since 1.1.0.0
-}
withTiesFetchClauseModifier :: FetchClauseModifier
withTiesFetchClauseModifier =
  FetchClauseModifier (RawSql.fromString "WITH TIES")

{- | Disallow a FETCH clause to have ties.

@since 1.1.0.0
-}
onlyFetchClauseModifier :: FetchClauseModifier
onlyFetchClauseModifier =
  FetchClauseModifier (RawSql.fromString "ONLY")
