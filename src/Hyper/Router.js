exports._addRoutes = function (r) {
  return function (conn) {
    return function (success, error) {
      conn.components.routes = r;
      success(conn);
    }
  };
};
