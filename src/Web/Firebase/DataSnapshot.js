'use strict';

// module Web.Firebase.DataSnapshot

exports.valImpl = function (ds) {
    return ds.val();
};

exports._hasChild = function(ds, named) {
    return ds.hasChild(named);
};
