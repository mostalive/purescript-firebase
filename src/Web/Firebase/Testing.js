firebase = require("@firebase/testing");

exports._adminApp= function(databaseName) {
  return function (onError, onSuccess) {
    var app = firebase.initializeAdminApp({ databaseName: databaseName});
    onSuccess(app);
  };

  //canceler always succeeds
  return function (cancelError, cancelerError, cancelerSuccess) {
    cancelerSuccess(); // invoke the success callback for the canceler
  };
};
