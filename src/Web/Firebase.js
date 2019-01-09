'use strict';
// var Firebase = require('firebase');
// module Web.Firebase

exports.newFirebaseImpl = function (uri) {
    return function () {
        return new Firebase(uri);
    };
};

exports.childImpl = function (childPath, aRef) {
  return function () {
    console.log("childImpl, path is': " + childPath);
    console.log("aRef is': " + aRef.toString());

    // childRef = windowindow.firebase.database().ref("roles/Oi6rJVtl6UMIfFI8CnCTSy0n7X33")
    var childRef = aRef.child(childPath);
    console.log("Crash?");
    console.log("childImpl, firebase ref is:" + childRef.toString());
    return childRef;
    };
};

exports._key = function(firebaseRef) {
  return function() {
    return firebaseRef.key();
  };
};

exports.onImpl = function (eventType, onComplete, onCancel, fb) {
  var successEffect  = function (snapshot) { // extract to withEffect1(fn, value)
    console.log("On " + eventType + " triggered. Snapshot was:");
    console.log(snapshot);
    onComplete(snapshot)();
  };
  var errorEffect  = function (error) {
    console.log("error subscribing on: " + eventType + " reference was: " + fb.toString());
    onCancel(error)();
  };

  console.log("Might JS subscribe On event type: " + eventType + ".");
  return function () {
    console.log("JS subscribing On " + eventType);
    console.log("Ref to subscribe On: " + fb.toString());
    fb.on(eventType, successEffect, errorEffect);
  };
};

exports.onWithoutCancelCallbackImpl = function (eventType, callback, fb) {
    console.log("without error callback, should be possible according to documentation");
    return function () {
        return fb.on(eventType, callback);
    };
};

exports._offSimple = function (fb) {
  return function() {
    return fb.off();
  };
};

exports.onceImpl = function(eventType, callback, errorCallback, fb) {
    var logDecorator = function(error) {
      errorCallback(error)();
    };
    return function () {
      fb.once(eventType, callback, logDecorator);
    };
};

exports.setImpl = function (value, onComplete, fb) {
    return function () {
        fb.set(value, onComplete === null ? undefined : onComplete);
    };
};

// onComplete passes null for error when success, and a FirebaseErr on error
exports.setEImpl = function (value, onComplete, fb) {
    var runEffect  = function (error) {
      console.log("setEImpl");
      console.log(error);
      onComplete(error)();
    };
    return function () {
        fb.set(value, runEffect);
    };
};
// extra to firebase api, easy Aff bridge, explicit callbacks
exports._setA = function (value, onSuccess, onError, fb) {
    return function () {
      var newRef = null;
      var runEffect  = function (error) {
        if(error)
          onError(error)();
        else
          onSuccess()();
      };
      newRef = fb.set(value, runEffect);
      return newRef;
    };
};
/*
 * https://www.firebase.com/docs/web/api/firebase/push.html
 * Generate a new child location using a unique name and returns a Firebase reference to it. This is useful when the children of a database location represent a collection of items. See Saving Lists of Data.
 *
 * You can optionally pass a value to push() and the value will be immediately written to the generated location. If you don't pass a value to push(), nothing is written and the child will remain empty unless written to using set().
 *
 * The unique name generated by push() is prefixed with a client-generated timestamp so that the resulting list will be chronologically-sorted.
 *
 */

exports.pushImpl = function (value, onError, fb) {
    var runEffect  = function (error) {
      onError(error)();
    };
    return function () {
        return fb.push(value, onError === null ? undefined : runEffect);
    };
};

// extra to firebase api, explicit error handling
exports.pushEImpl = function (value, onError, fb) {
    var runEffect  = function (error) {
      onError(error)();
    };
    return function () {
        return fb.push(value, runEffect);
    };
};

var withEffect1  = function (f, value) {
    return  f(value)();
};
// extra to firebase api, easy Aff bridge, explicit callbacks
exports._pushA = function (value, onSuccess, onError, fb) {
    return function () {
      var newRef = null;
      var runEffect  = function (error) {
        if(error)
          onError(error)();
        else
          onSuccess(newRef)();
      };
      newRef = fb.push(value, runEffect);
      return newRef;
    };
};

exports._toString = function(firebaseRef) {
  return function() {
    return firebaseRef.toString();
  };
};
