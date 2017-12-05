"use strict";

exports.generatedSessionID = function() {
  return String((new Date()).getTime() + Math.random());
};
