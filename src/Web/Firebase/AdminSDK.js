const admin = require('firebase-admin');

exports.requireAdminSDKImpl = admin;

exports.initializeAppImpl = function(certJSON, databaseName, admin ) {
  const serviceAccount = JSON.parse(certJSON);
  return function() {return admin.initializeApp({
    credential: admin.credential.cert(serviceAccount),
    databaseURL: ("https://" + databaseName + ".firebaseio.com")
  });};
}

exports.createCustomTokenImpl = function(uid, admin) {
  return function() {
    admin.auth().createCustomToken(uid)
      .then(function(customToken) {
        return customToken;
      })
      .catch(function(error) {
        throw ("Error creating custom token:", error);
      });
  };
}
