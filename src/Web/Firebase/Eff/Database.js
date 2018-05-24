exports.app = function(database) { return database.app;};

exports.rootRefForImpl = function (database) { return database.ref(); };

exports.refImpl = function (path, database) { return database.ref(path); };

exports.goOfflineImpl = function (database) {database.goOffline();};

exports.goOnlineImpl = function (database) {database.goOnline();};
