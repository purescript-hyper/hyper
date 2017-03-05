.PHONY: docs
docs:
	make -C docs-src release

.PHONY: examples
examples:
	pulp build -I docs-src/src/topic-guides
	pulp build -I examples
