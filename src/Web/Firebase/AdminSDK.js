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

exports.everythingInJsImpl = function(jsonFileName) {
  return function(onError, onSuccess) {
    const serviceAccount = require(jsonFileName);
    const admin = adm;
    const databaseName = "purescript-spike";
    console.log("admin is: " + admin);
      admin.initializeApp({
        credential: admin.credential.cert(serviceAccount),
        databaseURL: ("https://" + databaseName + ".firebaseio.com")
      });

      console.log("admin app initialized" + admin.app().name);
     return createCustomTokenInner("fakeuid",admin,onError, onSuccess);
  };
};

exports.createCustomTokenImpl = function(uid) {
  console.log('create custom token called');
  return function (admin) {
    return function(onError, onSuccess) {
    console.log('on top of createCustomToken, admin is' + admin);
      console.log('function called');
      admin.auth().createCustomToken(uid).then(function(customToken) {
          console.log("custom Token: " + customToken);
          onSuccess(customToken);
        })
        .catch(function(error) {
          console.log("error" + error);
          onError(error);
        });
    };
  };
};

createCustomTokenInner = function(uid,admin,onError,onSuccess) {
  console.log('on top of createCustomToken, admin is' + admin);
  console.log('function called');
  admin.auth().createCustomToken(uid).then(function(customToken) {
    console.log("custom Token: " + customToken);
    onSuccess(customToken);
  })
    .catch(function(error) {
      console.log("error" + error);
      onError(error);
    });
};
