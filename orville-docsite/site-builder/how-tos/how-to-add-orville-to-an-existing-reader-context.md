---
title: How To Add Orville to an Existing Reader Context
navOrder: 2
---

This guide will show you how to add Orville to a monad that is already using
`ReaderT` in its monad stack. It builds conceptually on top of [the previous
guide](how-to-add-orville-to-your-application-monad.html), which assumed there
was not already a `ReaderT` in the application monad stack. It's recommended
that you read that guide before this one.

The file listing below shows a simple application with its own `Application`
monad that already has a reader context. When there is already a reader context
in the application stack it's generally perferrable to incorporate Orville into
the existing reader context rather than adding a new `ReaderT` layer atop the
one that's already there.

$sample("adding-orville-existing-readert/snapshots/Main-1.hs", "filename=Main.hs (Before)")$

As in [the last guide](how-to-add-orville-to-your-application-monad.html), we
will first add an `OrvilleState` to our application monad. In this case we'll
add it as a new field to the existing `ApplicationContext`.

$sample("adding-orville-existing-readert/1-add-orville-state.patch", "filename=Main.hs")$

This requires that the new `applicationOrvilleState` field be populated in the
`runApplication` function using a `ConnectionPool`. The `ConnectionPool` is
created in the `main` function and passed in where `runApplication` is called.

$sample("adding-orville-existing-readert/2-update-runApplication.patch", "filename=Main.hs")$

Now we must declare an instance of `HasOrvilleState` to allow Orville access to
the `OrvilleState` state that is stored in our custom `ApplicationContext`
field. The `askOrvilleState` function is generally quite easy to implement.
It's the equivalent of the `ask` function from the `Reader` module. In this
example we use the `asks` function from the `Reader` module to access the
`applicationOrvilleState` field in the reader context.

The `localOrvilleState` function is the equivalent of the `local` function from
the `Reader` module. It's slightly more complicated to implemented because we
have adapt the function that Orville passes to `localOrvilleState` (which has type
`OrvilleState -> OrvilleState`) so that the function is applied within the
`ApplicationContext`. The adapted function is then passed `Reader.local` to
complete our implementation of `localOrvilleState`. We've included a type
signature for `mkLocalContext` in the example so you can clearly see the type
of function being passed to `Reader.local`, but this is not necessary for the
code to compile.

$sample("adding-orville-existing-readert/3-add-HasOrvilleState.patch", "filename=Main.hs")$

Once we have defined our instance of `HasOrvilleState` we can add
`MonadOrville` and `MonadOrvilleControl` to the list of derived instances for
`Application`.

$sample("adding-orville-existing-readert/4-add-remaining-typeclasses.patch", "filename=Main.hs")$

Now our `Application` monad is fully equipped with Orville capabilities! [The
previous guide](how-to-add-orville-to-your-application-monad.html) showed how
to add a first table and Orville operation as well. That part is exactly the
same from this point, so we won't include it again here. We'll conclude this
guide with the final listing of `Main.hs` with all our changes applied.

$sample("adding-orville-existing-readert/snapshots/Main-5.hs", "filename=Main.hs (After)")$
