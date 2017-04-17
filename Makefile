VERSION ?= $(shell git rev-parse --short HEAD)

.PHONY: docs
docs:
	make -C docs release

deploy-docs: docs
	aws s3 sync --region=eu-west-1 docs/target/release s3://hyper.wickstrom.tech/docs/$(VERSION)

.PHONY: examples
examples:
	# Disabled! This part of the docs should be moved to the respective
	# repository instead.
	# make -C docs/src/extensions/type-level-routing/examples build
	#
	pulp build -I docs/src/topic-guides
	pulp build -I examples
