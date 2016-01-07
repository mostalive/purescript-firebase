'use strict';

// module Web.Firebase.DataSnapshot

exports._exists = function(ds) {
    return ds.exists();
};

exports.valImpl = function (ds) {
    return ds.val();
};

exports._hasChild = function(ds, named) {
    var really = ds.hasChild(named);
    console.log(really);
    return really;
};

exports._hasChildren = function(ds) {
  return ds.hasChildren();
};

exports._numChildren = function(ds) {
  return ds.numChildren();
};
