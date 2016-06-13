## Module Control.Safely

#### `safely`

``` purescript
safely :: forall m a. MonadRec m => (forall safe. MonadRec safe => (m ~> safe) -> (safe ~> m) -> safe a) -> m a
```

Make a control operator stack-safe.

For any `MonadRec`, we can form the `Monad` `FreeT Identity m` in which
arbitrarily-associated binds are stack-safe.

This function performs the wrapping and unwrapping for us, and provides a
pair of natural transformations to lift and lower actions to and from the
safe monad.

This means that we can write stack-safe monadic code for any `MonadRec` by
simply writing the naive implementation, and wrapping it in a call to
`safely`:

```purescript
traverseS_ :: forall m a. MonadRec m => (a -> m Unit) -> List a -> m Unit
traverseS_ f xs = safely \lift lower ->
  let
    go Nil = pure unit
    go (Cons x xs) = do
      lift (f x)
      go xs
  in go xs
```

#### `replicateS_`

``` purescript
replicateS_ :: forall m a. MonadRec m => Int -> m a -> m Unit
```

Safely replicate an action N times.

#### `traverseS_`

``` purescript
traverseS_ :: forall f m a. (Foldable f, MonadRec m) => (a -> m Unit) -> f a -> m Unit
```

Safely traverse a foldable container.

#### `foldS`

``` purescript
foldS :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> List b -> m a
```

Perform a monadic fold, safely.


