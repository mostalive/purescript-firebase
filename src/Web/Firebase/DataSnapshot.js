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

exports._hasChildren = function(ds) {
  return ds.hasChildren();
};

exports._key = function(ds) {
  return ds.key();
};

exports._numChildren = function(ds) {
 return ds.numChildren();
};
