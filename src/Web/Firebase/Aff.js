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
