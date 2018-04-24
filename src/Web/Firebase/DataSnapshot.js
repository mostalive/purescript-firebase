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

exports._child = function(ds, named) {
 return ds.child(named);
};

exports._numChildren = function(ds) {
  console.log("numchildren called");
  console.log(ds);
 return ds.numChildren();
};

exports._key = function(ds) {
  console.log("key called");
  const key = ds.key;
  console.log(key);
  return key;
};
