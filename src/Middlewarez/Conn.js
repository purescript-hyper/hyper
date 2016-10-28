exports.unsafeParseBody = function (f) {
    return function (c) {
        // Random temporary stuff, just to test.
        console.log(c);
        return {
            body: f("testing")
        };
    };
};
