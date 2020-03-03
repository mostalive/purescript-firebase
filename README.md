# Purescript-firebase

[![Latest release](http://img.shields.io/bower/v/purescript-firebase.svg)](https://github.com/mostalive/purescript-firebase/releases)
[![Build Status](https://travis-ci.org/mostalive/purescript-firebase.svg?branch=master)](https://travis-ci.org/mostalive/purescript-firebase)

Purescript bindings for firebase realtime database.

## Status

[WeReview](https://wereviewhq.com) uses an older version of this in production. Currently upgrading tests so they run with an in-memory, local firebase. Therefore not all tests are included in the build at the moment. They will be added one by one as they are converted.  

Pull requests are welcome, for additions and fixes. If possible, with tests. If you can't figure it out, happy to discuss in an issue.

## Installing

```
bower install purescript-firebase
```

## Building

```
pulp -w test
```

should work. For instructions on how to build from scratch, see '.travis.yml'.

## Security

There is a set of rules in test/rules.bolt, they are meant to test access denied errors. If you want to do your own experiments with it, change the database in 'test/Main.purs' and 'firebase.json' to one that you control, generate security rules with the 'bolt' compiler with

```
npm run bolt
```

and deploy the security rules

```
firebase deploy:rules
```

You'll notice that one path is open for writing by default.

## Style

The [purescript style guide](https://github.com/purescript/purescript/wiki/Style-Guide) recommends separating a literal translation of the API in a separate package / repository from a more purescript-ish interface. For now they are together, until we figure out what goes where. Web.Firebase.Monad.Aff has the start of a more idiomatic API. Eventually I'd like to produce signals for callbacks that can be fired more than once ( on()). That way it should be relatively easy to use firebase for uni-directional data flow with one of the many signal-based UI libraries for purescript.


Pull requests welcome. The tests will tell you what's been done so far, and what I'm thinking about doing next.
## Bumping versions

* Commit, push, wait for CI to build clean
* Update CHANGELOG.md
* Bump [semantic version](http://semver.org/) with [bower version](https://bower.io/docs/api/) and push tags:


```
> bower version major
> git push origin master && git push --tags
```



# Credits

[Pascal Hartig](https://github.com/passy) - The only purescript code I could find that was using firebase was this https://github.com/passy/giflib . I extracted the firebase code and started working from there.
