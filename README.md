<div align="center">
<h1>
<img src="docs/src/_static/hyper@2x.png"
      alt="Hyper"
      width="300">
</h1>
</div>

<p align="center">
<em>Type-safe, statically checked composition of HTTP servers</em>
</p>

<p align="center">
<a href="https://hyper.wickstrom.tech/docs/v0.8.0/tutorials/getting-started-with-hyper.html">Getting Started</a>
| <a href="https://hyper.wickstrom.tech">Documentation</a>
| <a href="https://hyper.wickstrom.tech/docs/v0.8.0/faq.html">FAQ</a>
| <a href="examples/">Examples</a>
</p>

<hr>

Hyper is an experimental middleware architecture for HTTP servers written in PureScript. Its main focus is correctness and type-safety, using type-level information to enforce correct composition and abstraction for web servers. The Hyper project is also a breeding ground for higher-level web server constructs, which tend to fall under the “framework” category.

To learn
more about Hyper, check out the [documentation](https://hyper.wickstrom.tech)
and [the Getting Started
tutorial](https://hyper.wickstrom.tech/docs/v0.8.0/tutorials/getting-started-with-hyper.html).

<p align="right">
<a href="https://travis-ci.org/owickstrom/hyper"><img alt="Build Status" src="https://travis-ci.org/owickstrom/hyper.svg?branch=master" /></a>
<a href="https://github.com/owickstrom/hyper/releases"><img alt="Latest release" src="https://img.shields.io/github/release/owickstrom/hyper.svg" /></a>

</p>

<hr>

## Prerequisites

* PureScript 0.10.6 or higher
* NodeJS
* Bower
* Pulp

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
# general format:
pulp run -I examples --main Examples.<example-name>

# for instance to run HelloHyper:
pulp run -I examples --main Examples.HelloHyper
```

### Building all Examples

```bash
make examples
```

### Building Documentation

See [docs/README.md](docs/README.md) for prerequisites, setup, and
how to work with the documentation.

Then, for the release build, run:

```bash
make docs
```

## CodeScene Status

[![](https://codescene.io/projects/49/status.svg) Get more details at **codescene.io**.](https://codescene.io/projects/49/jobs/latest-successful/results)

## License

[Mozilla Public License Version 2.0](LICENSE)
