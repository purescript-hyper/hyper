"use strict";

var crypto = require('crypto')

exports.randString = function () {
  return crypto.randomBytes(32).toString('hex');
}
