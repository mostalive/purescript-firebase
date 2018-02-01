"use strict";

// https://www.firebase.com/docs/web/guide/user-auth.html#section-handling-errors
// firebase error message has a code (string) message (string) and possibly details (string)
// all are attributes
exports.fb2error = function(fberr) {
 var message = fberr.message + "\n | firebase code: | \n " + fberr.code;
 if(fberr.details) // abuse truthyness of null and undefined
   message += "\n | details | \n" | fberr.details;
 return new Error(message);
};

exports.firebaseErrToString = function(fberr) {
 var message = fberr.message + "\n | firebase code: | \n " + fberr.code;
 if(fberr.details) // abuse truthyness of null and undefined
   message += "\n | details | \n" | fberr.details;
 return message;
};

// implemented separately because of new Aff -  https://github.com/slamdata/purescript-aff
exports._on = function (eventType, ref) { // accepts a request
  return function(onError, onSuccess) { // and callbacks
    var req = ref.on(eventType, onSuccess, onError );

    // returns a canceler, which is just another Aff efect
    return function (cancelError, cancelerError, cancelerSuccess) {
      ref.off();
      cancelerSuccess();
    };
  };

};

exports._once = function(interest) {

};

exports._push = function(saveable) {};

exports._remove = function(location) {

};

exports._set = function (saveable) {};
