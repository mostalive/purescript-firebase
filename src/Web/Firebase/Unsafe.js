'use strict';

// module Web.Firebase.Unsafe

exports.unsafeEvalEff = function (f) {
    f();
    return f;
};
