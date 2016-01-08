## Module Web.Firebase.Monad.Aff

#### `on`

``` purescript
on :: forall eff. EventType -> Firebase -> Aff (firebase :: FirebaseEff | eff) DataSnapshot
```

#### `once`

``` purescript
once :: forall e. EventType -> Firebase -> Aff (firebase :: FirebaseEff | e) DataSnapshot
```

#### `onceValue`

``` purescript
onceValue :: forall e. Firebase -> Aff (firebase :: FirebaseEff | e) DataSnapshot
```


