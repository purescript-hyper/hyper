// var rawBody = require('raw-body');

exports.parseBodyFromString = function (f) {
  return function (conn) {
       if (conn.request.body) {
      throw new Error('.request.body already set on Conn!');
    }

    conn.request.body = f('');

    return conn;
  };
};
