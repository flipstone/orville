---
title: Home
---

Orville's goal is to provide a powerful API for applications to access
PostgreSQL databases with minimal use of sophisticated language techniques or
extensions. It strikes a balance between enforcing type-safety in database
interactions where it is reasonable and presenting type signatures that are
minimally complicated.

## Why Orville?

Orville is not meant to replace existing PostgreSQL libraries in the Haskell
ecosystem, but to complement them. It has the power to satisfy most experienced
Haskell developers but strives to remain approachable to newcomers despite
this. Orville's API is rich enough to be used in production on large and
sophisticated applications, but avoids complicated type-level programming. If
your application is too large to reasonably write all your SQL statements by
hand yet doesn't require absolute type-safety between your custom SQL
statements, their result sets and the Haskell types they decode into, Orville
may be the right choice for you.

## Feature Overview

* Rich API for marshalling Haskell types to and from SQL
* High-level APIs for common CRUD operations
* Optional automatic schema migrations
* Optional API for executing complex data loads across multiple tables without ever writing an N+1 query by accident
* Progressive escape hatches to let you dig deeper when you need to

## Tutorials

See the tutorials, in order of increasing complexity:

$for(tutorials)$
* <a href="$url$">$title$</a>$endfor$

Additional documentation is available in the Haddocks.

## Just show me some code!

Ok! Here's a very simple application that inserts some entities of a `Pet`
model and finds one of them based on its name.

$sample("hero/src/Main.hs", "filename=")$
