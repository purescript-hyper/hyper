exports._addRoutes = function (routes) {
  return function (conn) {
    conn.components.routes = routes;
    return conn;
  };
};

function findRoute(routes, segments, method) {
  if (segments && segments.length > 0 && segments[0] === '') {
    return routes[method];
  }
  
  var first = segments[0];
  var rest = segments.slice(1);
  return findRoute(routes[first], rest, method);

}

exports._runRoutes = function (routes) {
  return function (method) {
    return function (conn) {
      return function (success, error) {
        try {
          var route = findRoute(routes,
                                conn.request.path.split('/'),
                                method);
          if (route) {
            route(conn)(success, error);
          } else {
            throw new Error('No matching route for ' + method + ' on path \'' + conn.request.path + '\'');
          }
        } catch (e) {
          error(e);
        }
      };
    };
  };
};
