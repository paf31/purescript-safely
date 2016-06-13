# purescript-safely

A combinator for making monadic controls operator stack-safe (using `MonadRec`).

For example, `purescript-lists` defines `replicateM_`, which is not stack-safe. It gives a stack overflow when applied to large inputs using, for example, the `Eff` monad.

```text
> replicateM_ 100000 (log "Testing...")

RangeError: Maximum call stack size exceeded
```

However, we can make it stack-safe by using the `safely` combinator.

For any `MonadRec`, we can form the `Monad` `FreeT Identity m` in which
arbitrarily-associated binds are stack-safe.

The `safely` function performs the wrapping and unwrapping for us, and provides a
pair of natural transformations to lift and lower actions to and from the
safe monad.

This means that we can write stack-safe monadic code for any `MonadRec` by
simply writing the naive implementation, and wrapping it in a call to
`safely`:

```purescript
replicateS_ :: forall m a. MonadRec m => Int -> m a -> m Unit
replicateS_ n x = safely \lift _ ->
  let go i | i <= 0 = pure unit
           | otherwise = do lift x
                            go (i - 1)
  in go n
```
