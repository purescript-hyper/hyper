var rawBody = require('raw-body');
var typer = require('media-typer');

exports._parseBodyAsString = function (conn) {
  return function (error) {
    return function (success) {
      return function () {
        if (conn.request.body) {
          return error(new Error('.request.body already set on Conn!'));
        }
        try {
          var charset = typer.parse(conn.request.headers['content-type']).parameters.charset;
          rawBody(conn.request.bodyStream, {
            length: parseInt(conn.request.headers['content-length']),
            limit: '1mb',
            encoding: charset
          }, function (err, str) {
            if (err) {
              error(err)();
            } else {
              conn.request.body = str;
              success(conn)();
            }
          });
        } catch (e) {
          error(e)();
        }
      };
    };
  };
};
