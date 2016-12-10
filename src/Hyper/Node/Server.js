// module Hyper.Node.Server

var http = require('http');

exports._end = function (res) {
  return function () {
    res.end(s);
  };
};

exports._serveHttp = function () {
  return function (port) {
    return function (handler) {
      return function () {
        var server = http.createServer(function (req, res) {
          var result = handler({
            request: req,
            response: res
          })();
        });
        
        server.on('clientError', function (err, socket) {
          socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
        });

        server.listen(port);
      };
    };
  };
};
