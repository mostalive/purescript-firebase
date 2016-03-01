'use strict';

// module Web.Firebase.Authentication.Eff


exports._onAuth = function (callback, firebase) {
  var cbEffect = function(data) {
    return cbEffect(data)(); // ensure effect gets used
  };
  return function() {
    return firebase.onAuth(cbEffect);
  };
};

exports._authWithOAuthRedirect = function (provider, errorCallback, ref) {
  var errorCbEffect = function(error) {
    return errorCallback(error)(); // ensure effect gets used
  };
  ref.authWithOAuthRedirect(provider, errorCbEffect);
};
