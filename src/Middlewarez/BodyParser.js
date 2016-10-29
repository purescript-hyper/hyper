// var rawBody = require('raw-body');

exports._parseBodyFromString = function (f) {
  return function (conn) {
    return function (error) {
      return function (success) {
        return function () {
          if (conn.request.body) {
            return error(new Error('.request.body already set on Conn!'));
          }
          console.log(conn.request.headers);
          conn.request.body = f('');

          return success(conn);
        };
      };
    };
  };
};
