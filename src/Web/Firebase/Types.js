
"use strict";


exports.firebaseErrToString = function(fberr) {
 var message = fberr.message + "\n | firebase code: | \n " + fberr.code;
 if(fberr.details) // abuse truthyness of null and undefined
   message += "\n | details | \n" | fberr.details;
 return message;
};
