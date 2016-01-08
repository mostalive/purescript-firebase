## Module Web.Firebase

#### `newFirebase`

``` purescript
newFirebase :: forall eff. URI -> Eff (firebase :: FirebaseEff | eff) Firebase
```

#### `child`

``` purescript
child :: forall eff. String -> Firebase -> Eff (firebase :: FirebaseEff | eff) Firebase
```

#### `EventType`

``` purescript
data EventType
  = Value
  | ChildAdded
  | ChildChanged
  | ChildRemoved
  | ChildMoved
```

#### `on`

``` purescript
on :: forall eff. EventType -> (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) -> Maybe (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit) -> Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
```

#### `once`

``` purescript
once :: forall eff. EventType -> (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) -> Maybe (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit) -> Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
```

#### `set`

``` purescript
set :: forall eff. Foreign -> Maybe (Maybe (FirebaseErr -> Eff eff Unit)) -> Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
```

#### `push`

``` purescript
push :: forall eff. Foreign -> Maybe (Maybe (FirebaseErr -> Eff eff Unit)) -> Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
```


