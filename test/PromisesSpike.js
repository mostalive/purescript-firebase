const q = require('q');

exports.concat0LiftImpl = function() {
    return "zero";
};

exports.notCurriedImpl = function(one,two) {
  return one + two;
};

exports.concat1LiftImpl = function(aString) {
  return function() {return aString;};
};

exports.concat2LiftImpl = function(one) {
  return function(two) {
    return function() {return one + two;}
  ;};
};

exports.promiseConcat1Impl = function(msg) {
  return function(err,response) {
    // for example
    q.delay(100).then(function() {
      return msg;
    }).fail(err).then(response);
  };
};

exports.promiseConcat0Impl = function(onError,onSuccess) {
    // for example
    q.delay(100).then(function() {
      return "zero";
    }).fail(onError).then(onSuccess);
};

exports.promiseConcat2Impl = function(msg) {
  return function (second) {
    return function(onError,onSuccess) {
      // for example
      q.delay(100).then(function() {
        return msg + second;
      }).fail(onError).then(onSuccess);
    };
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

exports.concat2FromEffnAffImpl = function(one) {
  return function(two) {
    return function(onError,onSuccess) {
      setTimeout(onSuccess,100,one+two);
      return function(cancelError, cancelerError, cancelerSuccess) {
        console.log('canceler invoked');
        cancelerSuccess();
      };
    };
  };
};
