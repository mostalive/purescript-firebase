# Change Log

## [v 1.1.0] = 2016-07-01

 * compatible with purescript 0.9.1 and 1.0.x version of libraries
 * Removed Eff tests with AVars - hard to write and modify, behaviour tested was also indirectly covered in Aff tests.

### Fixed

* Use <> instead of ++ and pure instead of return
* Set and Push (in Aff) now have firebase reference as last parameter for easier chaining
* Remove duplicate test WritingSpec

