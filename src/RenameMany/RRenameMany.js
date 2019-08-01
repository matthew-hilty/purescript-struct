"use strict";

exports.unsafeRenameManyBuilder = function (tuples, record) {
  var count = tuples.length;
  for (var i = 0; i < count; i++) {
    var tuple = tuples[i];
    var oldKey = tuple.value0;
    var newKey = tuple.value1;
    record[newKey] = record[oldKey]
    delete record[oldKey]
  }
  return record;
};

exports.unsafeRenameManyFunction = function (tuples, record) {
  var result = {};
  for (var key in record) {
    if ({}.hasOwnProperty.call(record, key)) {
      result[key] = record[key];
    }
  }
  var count = tuples.length;
  for (var i = 0; i < count; i++) {
    var tuple = tuples[i];
    var oldKey = tuple.value0;
    var newKey = tuple.value1;
    result[newKey] = result[oldKey];
    delete result[oldKey];
  }
  return result;
};
