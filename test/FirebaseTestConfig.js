exports.firebaseTestRef = function() {
  var config = {
    apiKey: "AIzaSyAyPNxC9QM8TsF_EfyyPVcPSpI5W8ARoa8",
    authDomain: "purescript-spike.firebaseapp.com",
    databaseURL: "https://purescript-spike.firebaseio.com",
    projectId: "purescript-spike",
    storageBucket: "purescript-spike.appspot.com",
    messagingSenderId: "770384530890"
  };
  firebase.initializeApp(config);

  return firebase.ref();

}
