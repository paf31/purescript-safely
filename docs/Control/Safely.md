## Module Control.Safely

#### `MMorph`

``` purescript
type MMorph f g = forall a. f a -> g a
```

A monad morphism.

#### `Operator`

``` purescript
class Operator o where
  mapO :: forall m n. MMorph m n -> MMorph n m -> o m -> o n
```

A control operator is an operator which works on any `Monad`,
so that we can change the monad by providing monad morphisms
to wrap and unwrap monadic actions.

#### `safely`

``` purescript
safely :: forall o m a. (Operator o, MonadRec m) => (forall t. (Monad t) => o t) -> o m
```

Make a control operator stack-safe.


