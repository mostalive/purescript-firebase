'use strict';

// module Web.Firebase.Authentication.Eff

exports._onAuth = function (callback, firebase) {
  return function() {
    return firebase.onAuth(callback);
  };
};

exports._authWithOAuthRedirect = function (provider, errorCallback, ref) {
  console.log("oAuth started");
  var errorCbEffect = function(error) {
    console.log("js callback called");
    return errorCallback(error)(); // ensure effect gets used
  };
  ref.authWithOAuthRedirect(provider, errorCbEffect);
  console.log("oAuth initiated in js");
};
