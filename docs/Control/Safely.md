## Module Control.Safely

#### `MMorph`

``` purescript
type MMorph f g = forall a. f a -> g a
```

A monad morphism.

#### `safely`

``` purescript
safely :: forall m a. (MonadRec m) => (forall t. (Monad t) => MMorph m t -> t a) -> m a
```

Make a monadic action stack-safe.


