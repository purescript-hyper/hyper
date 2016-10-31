exports._addRoutes = function (routes) {
  return function (components) {
    components.routes = routes;
    return components;
  };
};

exports._runRoutes = function (routes) {
  return function (conn) {
    console.log(routes.GET);
    conn.response.body = "Hello!";
    return conn;
  };
};
