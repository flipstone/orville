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

### Changed
- Changed policies are always dropped and recreated rather than altered, since
  `ALTER POLICY` cannot change a policy's permissive/restrictive setting or
  command, nor remove a `USING`/`WITH CHECK` clause, and recreation orders
  correctly around column additions and drops.
### Deprecated
### Removed
### Fixed
- `addTableConstraints`, `addTableIndexes` and `addTableTriggers` now keep the
  last item when a single call passes multiple items with the same migration
  key, as documented. Previously the first item in the list won.
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
