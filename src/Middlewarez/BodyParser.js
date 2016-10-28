exports.unsafeParseBody = function (f) {
  return function (c) {
    // Random temporary stuff, just to test.
    
    if (c.request.body) {
      throw new Error('.request.body already set on Conn!');
    }
    
    c.request.body = f("testing");

    return c;
  };
};
