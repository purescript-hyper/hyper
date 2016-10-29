var stream = require('stream');

exports.fromString = function (str) {
    var s = new stream.Readable();
    s.push(str);
    s.push(null);
    return s;
};
