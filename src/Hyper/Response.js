exports._respond = function (b) {
  return function (conn) {
    conn.response.body = b;
    return conn;
  };
};
