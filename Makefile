VERSION ?= $(shell git rev-parse --short HEAD)

.PHONY: docs
docs:
	make -C docs release

.PHONY: examples
examples:
	# Disabled! This part of the docs should be moved to the respective
	# repository instead.
	# make -C docs/src/extensions/type-level-routing/examples build
	#
	pulp build -I docs/src/topic-guides
	pulp build -I examples
