.PHONY: docs
docs:
	make -C docs-src release

.PHONY: examples
examples:
	# Disabled! This part of the docs should be moved to the respective
	# repository instead.
	# make -C docs-src/src/extensions/type-level-routing/examples build
	#
	pulp build -I docs-src/src/topic-guides
	pulp build -I examples
