"use strict";

// module PlayWithFFI

exports.encodeURIComponent = encodeURIComponent;

exports.unsafeHead = function(arr) {
  if (arr.length) {
    return arr[0];
  } else {
    throw new Error('unsafeHead: empty array');
  }
};
