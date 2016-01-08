## Module PlayWithFire

#### `Success`

``` purescript
newtype Success
  = Success { success :: String }
```

##### Instances
``` purescript
IsForeign Success
Show Success
Eq Success
```

#### `foreignErrorToString`

``` purescript
foreignErrorToString :: ForeignError -> String
```

#### `aSuccessHandler`

``` purescript
aSuccessHandler :: Foreign -> Eff (console :: CONSOLE) Unit
```

#### `writeWithFire`

``` purescript
writeWithFire :: forall e. Eff (console :: CONSOLE, firebase :: FirebaseEff | e) Unit
```

#### `printSnapshot`

``` purescript
printSnapshot :: forall e. DataSnapshot -> Eff (console :: CONSOLE, firebase :: FirebaseEff | e) Unit
```

#### `snapshot2success`

``` purescript
snapshot2success :: DataSnapshot -> Either String Success
```

#### `readSuccessAff`

``` purescript
readSuccessAff :: forall eff. Firebase -> Aff (firebase :: FirebaseEff | eff) (Either String Success)
```


