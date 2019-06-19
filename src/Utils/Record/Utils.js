"use strict";

exports.unsafeSingleton = function (label) {
  return function (value) {
    var record = {};
    record[label] = value;
    return record;
  };
};
