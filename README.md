# Hyper

*Type-safe, statically checked composition of HTTP servers, using PureScript.*

Hyper is an **experiment**, trying to improve correctness in web server
programming, and in HTTP middleware and request handlers specifically. To learn
more about Hyper, check out these resources:

* [Documentation](https://owickstrom.github.io/hyper/)
* ["Hyper: Elegant Weapons for a More Civilized Page"](https://owickstrom.github.io/hyper/) (introductory blog post)
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

## CodeScene Status

[![](https://codescene.io/projects/49/status.svg) Get more details at **codescene.io**.](https://codescene.io/projects/49/jobs/latest-successful/results)

## License

[Mozilla Public License Version 2.0](LICENSE)
