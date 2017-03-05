VERSION=$(shell git rev-parse --short HEAD)


.PHONY: docs
docs:
	make -C docs-src release

.PHONY: examples
examples:
	pulp build -I docs/src/basics
	pulp build -I docs/src/type-level-routing
	pulp build -I examples
