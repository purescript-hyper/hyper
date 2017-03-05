<div align="center">
<h1>
<img src="docs-src/src/_static/hyper@2x.png"
      alt="Hyper"
      width="300">
</h1>
</div>

<p align="center">
<em>Type-safe, statically checked composition of HTTP servers</em>
</p>

<p align="center">
<a href="https://wickstrom.tech/programming/2017/01/06/hyper-elegant-weapons-for-a-more-civilized-page.html">Introduction</a>
| <a href="https://owickstrom.github.io/hyper/">Documentation</a> (also in <a href="https://owickstrom.github.io/hyper/hyper.pdf">PDF</a>)
| <a href="examples/">Examples</a>
</p>

<hr>

Hyper is an experimental project, trying to improve correctness in web server
programming, and in HTTP middleware and request handlers specifically. To learn
more about Hyper, check out the
[documentation](http://hyper.wickstrom.tech), ["Hyper: Elegant Weapons
for a More Civilized
Page"](https://wickstrom.tech/programming/2017/01/06/hyper-elegant-weapons-for-a-more-civilized-page.html)
(introductory blog post), or the [examples](examples/).

<p align="right">
<a href="https://travis-ci.org/owickstrom/hyper"><img alt="Build Status" src="https://travis-ci.org/owickstrom/hyper.svg?branch=master" /></a>
<a href="https://github.com/owickstrom/hyper/releases"><img alt="Latest release" src="http://img.shields.io/github/release/owickstrom/hyper.svg" /></a>

</p>

<hr>

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
# for examples using the node server:
npm install
pulp run -I examples --main Examples.<example-name>
# for instance to run HelloHyper:
pulp run -I examples --main Examples.HelloHyper
```

### Building all Examples

```bash
npm install # for examples using the node server
make examples
```

### Building Documentation

See [docs-src/README.md](docs-src/README.md) for prerequisites, setup, and
how to work with the documentation.

Then, for the release build, run:

```bash
make docs
```

## CodeScene Status

[![](https://codescene.io/projects/49/status.svg) Get more details at **codescene.io**.](https://codescene.io/projects/49/jobs/latest-successful/results)

## License

[Mozilla Public License Version 2.0](LICENSE)
