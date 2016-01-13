# Purescript-firebase

[![Build Status](https://travis-ci.org/mostalive/purescript-firebase.svg?branch=master)](https://travis-ci.org/mostalive/purescript-firebase)

Purescript bindings for firebase.

Status: not for public consumption just yet. It eventually should follow the [purescript style guide](https://github.com/purescript/purescript/wiki/Style-Guide).

The style guide recommends separating a literal translation of the API in a separate package / repository from a more purescript-ish interface. For now they are together, until we figure out what goes where. Web.Firebase.Monad.Aff has the start of a more idiomatic API. Eventually I'd like to produce signals for callbacks that can be fired more than once ( on()). That way it should be relatively easy to use firebase for uni-directional data flow with one of the many signal-based UI libraries for purescript.

Pull requests welcome. The tests will tell you what's been done so far, and what I'm thinking about doing next.

# Credits

[Pascal Hartig](https://github.com/passy) - The only purescript code I could find that was using firebase was this https://github.com/passy/giflib . I extracted the firebase code and started working from there.
