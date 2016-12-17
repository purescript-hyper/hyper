# Hyper

*Type-safe, statically checked composition of HTTP servers, using PureScript.*

Hyper is an **experiment**, trying to improve correctness in web server
programming, and in HTTP middleware and request handlers specifically. Read
about the goals and design of Hyper at [owickstrom.github.io/hyper/](
https://owickstrom.github.io/hyper/).

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

## License

[Mozilla Public License Version 2.0](LICENSE)
