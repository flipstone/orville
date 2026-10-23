# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added
- Support for PostgreSQL row-level security policies: `mkPolicyDefinition` /
  `addTablePolicies` / `dropPolicies` on table definitions, with permissive or
  restrictive policies (`PolicyPermission`), command targeting
  (`PolicyCommand`), role targets including `PUBLIC`, `CURRENT_ROLE`,
  `CURRENT_USER` and `SESSION_USER` (`PolicyRole`), and `USING` / `WITH CHECK`
  expressions. Auto-migration creates, recreates and drops policies by
  comparing definitions against the `pg_policies` catalog view. Corresponding
  SQL builders are available in `Orville.PostgreSQL.Expr`.
- `setRowLevelSecurityEnabled` on table definitions. Auto-migration keeps the
  table's row-level security setting in sync with the definition (see the
  Security section below).
- Auto-migration rejects invalid policy definitions when generating a plan
  (throwing `MigrationDataError`) rather than failing mid-migration: policies
  both defined and marked for dropping, `USING` on `INSERT` policies,
  `WITH CHECK` on `SELECT`/`DELETE` policies, policy names over PostgreSQL's
  63-byte identifier limit, and policy expressions containing bind parameters.
- `fieldInSubquery` and `fieldNotInSubquery` (with `isInSubquery` and
  `isNotInSubquery` for any `SqlComparable`) check a field against the rows of a
  subquery.
- `groupByField` builds a `GroupByExpr` from a field's column, in the same way
  `orderByField` builds an `OrderByExpr`.
- `marshallAliasFields` qualifies the fields of a `SqlMarshaller` with an
  alias and selects each of them under a result name prefixed with that alias
  (`"b"."id" AS "b_id"`), decoding from the prefixed name. This lets the
  marshallers of several tables be combined to decode a query that joins those
  tables, even when the tables have columns with the same name.
  `aliasNameAndFieldNameToResultFieldName` builds the prefixed result names.
- `ReadOnlyTableDefinition` describes something rows can be selected from but
  not written to or migrated: a `TableReference` (usually a join) paired with
  the marshaller that decodes its rows. `mapReadOnlyTableMarshaller` reshapes
  the decoded entity.
- `Orville.PostgreSQL.Execution.SelectableEntityOperations` provides
  `findSelectableEntitiesBy` and `findFirstSelectableEntityBy`, which work
  with any `Selectable` definition. `findEntitiesBy` and `findFirstEntityBy`
  are their type specialized versions for `TableDefinition` and are unchanged.
- `ReadOnlyTableDefinition` describes a table or table-like item that can only
  be used in reads, such as a `JOIN`.
- `SomeFieldQualifier` is added to provide a generic way to reference a
  qualifier.
- `TableReferenceDefinition` describes a "table" (or a join of tables, or
  potentially other items such as a view) as it is referred to in a query
  involving more than one table: its `FROM` reference, the qualifier for its
  columns, the prefix for its result columns and its marshaller. It is built
  upon `ReadOnlyTableDefinition` and `SomeFieldQualifier`.
- `joinTables`, `joinOnFieldsEqual`, `innerJoinOnFieldsEqual`,
  `leftJoinOnFieldsEqual` and `joinOnCondition` build the `TableReference`
  for a join from `TableReferenceDefinition`s. `joinOnFieldsEqual` joins on
  the equality of two columns of the same Haskell type and qualifies the
  joined table's column itself; `joinOnCondition` accepts any `BooleanExpr`.
- `rowIdFieldNames` reads the row identifier field names of an
  `AnnotatedSqlMarshaller`.
- The `Selectable` class, with instances for `TableDefinition` and
  `ReadOnlyTableDefinition`, so a joined `ReadOnlyTableDefinition` can be
  queried with the same `SelectOptions` as a table.
- `selectMarshalledColumnsFromTableReference` builds a `Select` from a
  marshaller and any `TableReference`, where `selectMarshalledColumns` only
  accepts a table name.
- POSIX regular expression matching: `fieldMatchesRegex`,
  `fieldMatchesRegexInsensitive`, `fieldNotMatchesRegex` and
  `fieldNotMatchesRegexInsensitive`, with the corresponding `SqlComparable`
  functions, `BooleanExpr` builders (`matchesRegex` etc.) and binary operators
  (`regexMatchOp` etc.).

### Changed
- The `Natural` constructor of `MarshallerField` has a new argument after the
  `FieldDefinition` giving the name the field will have in a result set. It is
  the field's own name unless the field was aliased with `marshallAliasFields`.
  Code that pattern matches on `Natural` needs to accept the extra argument.
  `marshallerDerivedColumns` selects a field `AS` its result name whenever the
  two differ; for unaliased fields the generated SQL is unchanged.
- `selectTable` is now a method of the `Selectable` class. Existing calls with
  a `TableDefinition` are unchanged.
- Changed policies are always dropped and recreated rather than altered, since
  `ALTER POLICY` cannot change a policy's permissive/restrictive setting or
  command, nor remove a `USING`/`WITH CHECK` clause, and recreation orders
  correctly around column additions and drops.
### Deprecated
### Removed
### Fixed
- Aggregate expressions with an `ORDER BY` clause now put a space before
  `ORDER BY`. Previously a bind parameter as the last argument (for example the
  delimiter of `string_agg`) produced `$1ORDER BY`, which PostgreSQL 15 and
  later reject.
- Decoding a large result set is much faster. Rows are now decoded in constant
  stack space; previously the stack grew by one frame per row, which made each
  decoded value cost several microseconds on results with tens of thousands of
  rows.
- `addTableConstraints`, `addTableIndexes` and `addTableTriggers` now keep the
  last item when a single call passes multiple items with the same migration
  key, as documented. Previously the first item in the list won.
- `integralDefault` now renders each default the way PostgreSQL does. It uses a
  bare literal for non-negative values within the 32-bit range, `::integer` for
  negative values within it, and `::bigint` outside it. Previously a default
  below the 32-bit range failed with an out-of-range error, and a default above
  it caused spurious auto-migrations.
### Security
- **Auto-migration now manages row-level security on migrated tables.** A
  table definition that does not call `setRowLevelSecurityEnabled` declares
  row-level security disabled, and auto-migration will actively run
  `ALTER TABLE ... DISABLE ROW LEVEL SECURITY` on such a table if RLS is
  currently enabled on it — including RLS that was enabled outside of Orville.
  If you rely on manually-enabled RLS on Orville-managed tables, add
  `setRowLevelSecurityEnabled` to those table definitions before upgrading.
  Tables that already have RLS disabled are untouched.

## [v1.0.0.0] - 2023-10-30

First official release.
