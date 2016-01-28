'use strict';
var Firebase = require('firebase');
// module Web.Firebase

exports.newFirebaseImpl = function (uri) {
    return function () {
        return new Firebase(uri);
    };
};

exports.childImpl = function (childPath, firebase) {
    return function () {
        return firebase.child(childPath);
    };
};

exports.onImpl = function (eventType, callback, cancelCallback, fb) {
    return function () {
        return fb.on(eventType, callback, cancelCallback);
    };
};

exports.onceImpl = function(eventType, callback, errorCallback, fb) {
    var logDecorator = function(error) {
      console.log("the read failed" + error.code);
      errorCallback(error);
    }
    return function () {
      fb.once(eventType, callback, logDecorator);
    };
};

exports.setImpl = function (value, onComplete, fb) {
    return function () {
        fb.set(value, onComplete === null ? undefined : onComplete);
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

exports.pushImpl = function (value, onComplete, fb) {
    return function () {
        return fb.push(value, onComplete === null ? undefined : onComplete);
    };
};
