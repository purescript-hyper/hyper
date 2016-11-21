exports._router = function (resource) {
  return function (method) {
    return function (conn) {
      return resource[method](conn);
    };
  };
};
