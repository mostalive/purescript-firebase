const adm = require('firebase-admin');

exports.requireAdminSDKImpl = adm;

exports.initializeAppImpl = function(certJSON, databaseName, admin ) {
  console.log("admin is: " + admin);
  const serviceAccount = JSON.parse(certJSON);
  return function() {
    admin.initializeApp({
      credential: admin.credential.cert(serviceAccount),
      databaseURL: ("https://" + databaseName + ".firebaseio.com")
    });

    console.log("admin app initialized" + admin.app().name);
    return admin.app();
  };
};

exports.everythingInJsImpl = function(certJSON) {
  const admin = adm;
  const databaseName = "purescript-spike";
  console.log("admin is: " + admin);
  const serviceAccount = JSON.parse(certJSON);
  return function() {
    admin.initializeApp({
      credential: admin.credential.cert(serviceAccount),
      databaseURL: ("https://" + databaseName + ".firebaseio.com")
    });

    console.log("admin app initialized" + admin.app().name);
    return exports.createCustomTokenImpl("fakeuid",admin);
  };
};

exports.createCustomTokenImpl = function(uid, admin) {
  console.log('on top of createCustomToken, admin is' + admin);
  return function() {
    console.log('function called');
    return admin.auth().createCustomToken(uid);
    /*  .then(function(customToken) {
        console.log("custom Token: " + customToken);
        return Promise.resolve({app: admin, token: customToken});
      })
      .catch(function(error) {
        console.log("error" + error);
        throw ("Error creating custom token:", error);
      }); */
  };
}
