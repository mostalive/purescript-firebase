## Module Exceptional

#### `readConfig`

``` purescript
readConfig :: forall eff. Eff (err :: EXCEPTION | eff) Unit
```

#### `printException`

``` purescript
printException :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
```

#### `playWithExceptions`

``` purescript
playWithExceptions :: forall eff. Eff (console :: CONSOLE | eff) Unit
```


