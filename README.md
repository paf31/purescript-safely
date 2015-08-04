# purescript-safely

A combinator for making any monadic control operator stack-safe (using `MonadRec`).

For example, `purescript-lists` defines `replicateM`, which is not stack-safe. It gives a stack overflow when applied to large inputs using, for example, the `Eff` monad.

```text
> replicateM 100000 (log "Testing...")

RangeError: Maximum call stack size exceeded
```

However, we can make it stack-safe by using the `safely` combinator. First, define a data type to wrap `replicateM`:

```purescript
newtype Replicator m = Replicator (forall a. Int -> m a -> m (List a))

runReplicator :: forall m a. Replicator m -> Int -> m a -> m (List a)
runReplicator (Replicator r) = r
```

We need to define an instance of the `Operator` class:

```purescript
instance replicator :: Operator Replicator where
  mapO to fro (Replicator r) = Replicator \n m -> to (r n (fro m))
```

`mapO` modifies the underlying monad using a pair of monad morphisms.

Now we can derive a stack-safe implementation:

```purescript
replicateS :: forall m a. (MonadRec m) => Int -> m a -> m (List a)
replicateS = runReplicator (safely (Replicator replicateM))
```

which works for large inputs:

```text
> replicateS 100000 (log "Testing...")

Testing...
Testing...
Testing...
...
unit
```

We can apply `safely` to lift all sorts of control operators like `iterateM`, `traverse`, `filterM`, etc.
