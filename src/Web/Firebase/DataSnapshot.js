'use strict';

// module Web.Firebase.DataSnapshot

exports._exists = function(ds) {
    return ds.exists();
};

exports.valImpl = function (ds) {
    return ds.val();
};

exports._hasChild = function(ds, named) {
    return ds.hasChild(named);
};

exports._numChildren = function(ds) {
  return ds.numChildren();
};
