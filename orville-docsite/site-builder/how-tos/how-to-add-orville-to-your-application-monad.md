---
title: How To Add Orville to Your Application Monad
navOrder: 1
---

This guide will show you how to add Orville to your already existing
application monad in the simplest way possible. It relies on the `mtl` package
in addition to `orville-postgresql`, so you should make sure these packages are
in your package dependencies in either your `.cabal` or `package.yaml` file.

The file listing below shows a simple, minimal application with its own
`Application` monad. Your application is certainly more complicated than this,
but you can think of the `IO` type below being whatever base monad or monad
transformer stack you already have in place.

$sample("adding-orville-new-readert/snapshots/Main-1.hs", "filename=Main.hs (Before)")$

We're going to add a new `ReaderT` transformer to the innards of the
`Application` newtype to hold Orville's `OrvilleState` parameter. We'll have to
import the `Control.Monad.Reader` module (from the `mtl` package) and the
`Orville.PostgreSQL` module (from the `orville-postgresql`) package to
reference these types.

$sample("adding-orville-new-readert/1-add-readert.patch", "filename=Main.hs")$

This new `ReaderT` context adds the internal state that Orville needs to do its
job. In order to use functions from the Orville package directly in your monad
it will need to provide instances for the three typeclasses that make up a
complete Orville monad - `MonadOrville`, `MonadOrvilleControl` and
`HasOrvilleState`. Luckily, it's a simple matter of adding these three
typeclasses the deriving list for `Application`. If you're not using `GHC2021`
you'll need the `GeneralizedNewtypeDeriving` language extension, as in the
example in this guide.

$sample("adding-orville-new-readert/2-add-orville-typeclasses.patch", "filename=Main.hs")$

Somewhere in your code you'll have a function similar to this example's
`runApplication` function. It needs to be updated to expect a `ConnectionPool`
argument. We'll use the connection pool to build a fresh new `OrvilleState` for
our `Reader` context. Then we can use `runReaderT` resolve the `ReaderT` layer
we added to our stack and get back whatever monad type was there before we
added Orville. In this case that's just the `IO` type. This means
`runApplication` will return the same type that it returned befored we added
Orville, it just requires the `ConnectionPool` parameter to do its job now.

$sample("adding-orville-new-readert/3-update-runApplication.patch", "filename=Main.hs")$

Since we added a parameter to `runApplication`, we need to go to each place
it's called and pass a `ConnectionPool` parameter now. In this guide that's
just a single place in the `main` function. We'll use `createConnectionPool` as
normal to make the pool we want to use.

$sample("adding-orville-new-readert/4-update-main.patch", "filename=Main.hs")$

Our `Application` monad is now fully equipped to run Orville operations! To
show it works, let's add a `messages` table and update the `myApplication`
logic to insert and retrieve a simple greeting message.

$sample("adding-orville-new-readert/5-add-table.patch", "filename=Main.hs")$

Finally, we'll equip our application with automatic migrations using Orville's
`Orville.PostgreSQL.Automigration` module so that the `messages` table will be
created before we try to access it.

$sample("adding-orville-new-readert/6-add-migrations.patch", "filename=Main.hs")$

That's it! That completes this guide about how to add Orville to your existing
application monad. To wrap things up, here's the final listing of `Main.hs`
with all the changes we made.

$sample("adding-orville-new-readert/snapshots/Main-7.hs", "filename=Main.hs (After)")$
