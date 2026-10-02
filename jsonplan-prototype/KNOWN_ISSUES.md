# Known issues

Remaining hardening items for the compiled execution path. Both are edge
cases rather than defects in the supported usage, and both are covered by the
`Test.Equivalence` suite's native-versus-compiled comparison if a fix (or a
regression) changes observable behavior.

## Root parameter is encoded once for all consuming steps

`rootParamEncoder` walks the plan to the first step whose argument is
`rootParam` and uses that field's `fieldValueToSqlValue` to render every
parameter row in the `VALUES` clause. Every other step that consumes
`rootParam` then casts that single text rendering to its own column type.

If two steps consume the root parameter through fields that share a Haskell
type but were built from `SqlType`s with different `sqlTypeToSql` renderings
(possible via `convertSqlType`/`tryConvertSqlType`), the compiled query uses
the first field's rendering for both, while native execution encodes the
parameter per step. The second step can then match different rows than native
execution would.

Possible fixes:

- Emit one encoded parameter column per distinct consuming field
  (`VALUES (0, enc1, enc2), ...`) and point each step's match condition at
  its own column.
- Or verify during the compile pre-checks that all `rootParam`-consuming
  fields produce identical renderings for the parameter values at hand, and
  refuse otherwise, in line with how `ColumnNotWireRenderable` and
  `RefFieldOnNonEntity` are reported.

## Generated CTE names can shadow user tables

The compiler names its CTEs `jp0`, `jp1`, ... which are valid lowercase
identifiers. A step body that references a real table named `jpN` (for an
`N` smaller than the step's own index) resolves the name to the earlier
generated CTE instead of the table, because CTE names take precedence within
the `WITH` query. The result is a column-not-found error or, if the shapes
happen to line up, wrong rows.

Possible fixes:

- Schema-qualify table references in step bodies, which makes them immune to
  CTE shadowing regardless of the CTE naming scheme.
- Or pick a less collidable CTE prefix. This shrinks the collision surface
  but cannot eliminate it, since any valid identifier is also a valid table
  name.

Schema qualification is the correct fix; Orville's
`TableDefinition`/`TableIdentifier` carry the schema when one is set, but
unqualified tables would still need the lookup to prefer the table, so the
qualification likely needs to be explicit (for example `public.` when no
schema is configured, at the cost of assuming the search path).
