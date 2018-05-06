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
    return onSuccess("zero");
  };
};

exports.concat0FromEffnAffImpl = function(onError,onSuccess) {
    setTimeout(onSuccess,100,'zero');
    return function(cancelError, cancelerError, cancelerSuccess) {
      console.log('canceler invoked');
      cancelerSuccess();
    };
};

exports.concat1FromEffnAffImpl = function(one) {
  return function(onError,onSuccess) {
    setTimeout(onSuccess,100,one);
    return function(cancelError, cancelerError, cancelerSuccess) {
      console.log('canceler invoked');
      cancelerSuccess();
    };
  };
};
