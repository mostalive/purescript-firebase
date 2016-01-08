## Module PlayWithFFI

#### `encodeURIComponent`

``` purescript
encodeURIComponent :: String -> String
```

#### `Success`

``` purescript
newtype Success
  = Success { success :: String }
```

##### Instances
``` purescript
IsForeign Success
Show Success
```

#### `unsafeHead`

``` purescript
unsafeHead :: forall a. Array a -> a
```

#### `playWithFFI`

``` purescript
playWithFFI :: forall e. Eff (console :: CONSOLE | e) Unit
```


