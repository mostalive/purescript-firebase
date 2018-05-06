exports.concat0LiftImpl = function() {
    return "zero";
};

exports.concat1LiftImpl = function(aString) {
  return function() {return aString;};
};

exports.concat2LiftImpl = function(one) {
  return function(two) {
    return function() {return one + two;}
  ;};
};

exports.promiseConcat0Impl = function() {
  return function(onSuccess,onError) {
    return onSuccess("zero"); // Yay! Everything went well!
  };
};
