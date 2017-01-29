# Hyper

*Type-safe, statically checked composition of HTTP servers, using PureScript.*

[![Build Status](https://travis-ci.org/owickstrom/hyper.svg?branch=master)](https://travis-ci.org/owickstrom/hyper)

<hr>

Hyper is an experimental project, trying to improve correctness in web server
programming, and in HTTP middleware and request handlers specifically. To learn
more about Hyper, check out these resources:

* [Documentation](https://owickstrom.github.io/hyper/)
* ["Hyper: Elegant Weapons for a More Civilized Page"](https://wickstrom.tech/programming/2017/01/06/hyper-elegant-weapons-for-a-more-civilized-page.html) (introductory blog post)
* [Examples](examples/)

## Build

Install dependencies and build:

```bash
bower install
pulp build
```

### Running Tests

```bash
pulp test
```

### Running Examples

```bash
npm install # for examples using the node server
pulp run -I examples/<example-name>
```

### Building all Examples

```bash
npm install # for examples using the node server
make examples
```


## CodeScene Status

[![](https://codescene.io/projects/49/status.svg) Get more details at **codescene.io**.](https://codescene.io/projects/49/jobs/latest-successful/results)

## License

[Mozilla Public License Version 2.0](LICENSE)
