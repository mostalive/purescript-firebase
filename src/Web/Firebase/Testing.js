firebase = require("@firebase/testing");

// Callback to use this when cancelling is not an option provided.
function alwaysSucceed(cancelError, cancelerError, cancelerSuccess) {
  cancelerSuccess();
};

exports._adminApp= function(databaseName) {
  return function (onError, onSuccess) {
    var app = firebase.initializeAdminApp({ databaseName: databaseName});
    onSuccess(app);
  };

  return alwaysSucceed;
};

exports._initializeTestApp = function(authRecord) {
  return function (onError, onSuccess) {
    var app = firebase.initializeTestApp(authRecord);
    onSuccess(app);
  };

  return alwaysSucceed;
};

exports._initializeAnonymousTestApp = function(databaseName) {
  return function (onError, onSuccess) {
    var authRecord = firebase.initializeTestApp({
      databaseName: databaseName,
      auth: { uid: null }
    });

    var app = firebase.initializeTestApp(authRecord);
    onSuccess(app);
  };

  return alwaysSucceed;
};