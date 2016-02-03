'use strict';

// module Web.Firebase.Authentication.Eff

exports._onAuth = function (callback, firebase) {
  return function() {
    return firebase.onAuth(callback);
  };
};

exports._authWithOAuthRedirect(provider, errorCallback, ref) {
  var errorCbEffect = function() {
    return errorCallback()(); // ensure effect gets used
  }
  ref.authWithOAuthRedirect("twitter", errorCbEffect);
}
