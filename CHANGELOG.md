# Change Log

## [v 2.0.1] = 2016-08-31

* retrieve URL of a database reference

## [v 2.0.0] = 2016-08-30

* move Web.Firebase.Monad.Aff to Web.Firebase.Aff
* move some functions to read values that are not part of the Firebase API, but pertain to purescript to Web.Firebase.Aff.Read
* add functions to read with a default value
* upgraded to purescript version 0.9.3 and version 1.1 of purescript-aff

## [v 1.1.0] = 2016-07-01

 * compatible with purescript 0.9.1 and 1.0.x version of libraries
 * Removed Eff tests with AVars - hard to write and modify, behaviour tested was also indirectly covered in Aff tests.

### Fixed

* Use <> instead of ++ and pure instead of return
* Set and Push (in Aff) now have firebase reference as last parameter for easier chaining
* Remove duplicate test WritingSpec

