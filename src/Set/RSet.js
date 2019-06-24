"use strict";

exports.unsafeBuilderSet = function (label) {
  return function (value) {
    return function (rec) {
      rec[label] = value;
      return rec;
    };
  };
}
