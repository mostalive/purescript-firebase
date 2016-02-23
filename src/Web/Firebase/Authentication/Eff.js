'use strict';

// module Web.Firebase.Authentication.Eff

exports._onAuth = function (callback, firebase) {
  return function() {
    return firebase.onAuth(callback);
  };
};

exports._authWithOAuthRedirect = function (provider, errorCallback, ref) {
  var errorCbEffect = function(error) {
    return errorCallback(error)(); // ensure effect gets used
  };
  ref.authWithOAuthRedirect(provider, errorCbEffect);
};
