'use strict';

// module Web.Firebase.Authentication.Eff


exports._onAuth = function (callback, firebase) {
  var cbEffect = function(data) {
    return callback(data)(); // ensure effect gets used
  };
  return function() {
    return firebase.onAuth(cbEffect);
  };
};

// https://www.firebase.com/docs/web/api/firebase/offauth.html
// unregister authentication notifcation
exports._offAuth = function (callback, firebase) {
  return function() {
    return firebase.onAuth(cbEffect);
  };
};

exports._authWithOAuthRedirect = function (provider, errorCallback, ref) {
  var errorCbEffect = function(error) {
    return errorCallback(error)(); // ensure effect gets used
  };
  return function() {
    return ref.authWithOAuthRedirect(provider, errorCbEffect);
  };
};

// shape is slightly different from firebase function,
// we chose to have a success and error callback everywhere, so that writing Aff calls is straightforward
// and the API is consistent. Firebase has some with success / error callbacks, and some, like this with null (for success) or an additional parameter
exports._authWithCustomToken = function (token, successCallback, errorCallback, ref) {
  var errorCbEffect = function(error, authData) {
    if (error) {
      return errorCallback(error)();
    }
    else {
      return successCallback(authData)(); // extra () to ensure effects get used
    }
  };
  return function() {
    return ref.authWithCustomToken(token, errorCbEffect);
  };
};

// https://firebase.google.com/docs/reference/js/firebase.auth.Auth
// signOut returns a promise containing void, not sure we capture that correctly here
exports._unAuth = function (firebaseAuth) {
  return function() {
    return firebaseAuth.signOut();
  };
};
