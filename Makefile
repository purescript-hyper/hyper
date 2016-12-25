VERSION=$(shell git rev-parse --short HEAD)

MD_SOURCES=docs/index.md \
					 docs/goals.md \
					 docs/design.md \
					 docs/request-body-parsing.md \
					 docs/resource-routing.md \
					 docs/contributing.md

.PHONY: docs
docs: docs/index.html docs/hyper.pdf

docs/index.html: $(MD_SOURCES) docs/template.html docs/docs.css docs/highlight.js
	pandoc $(SHARED_PANDOC_OPTIONS) \
		-t html5 \
		--standalone \
		-S \
		--toc \
		--chapters \
		"--metadata=subtitle:$(VERSION)" \
		--no-highlight \
		-c docs.css \
		-o docs/index.html \
		--base-header-level=2 \
		-V source-code-url:https://github.com/owickstrom/hyper \
		-V author-url:https://wickstrom.tech \
		-V 'license:Mozilla Public License 2.0' \
		-V license-url:https://raw.githubusercontent.com/owickstrom/hyper/master/LICENSE \
		--template=docs/template.html \
	$(MD_SOURCES)

docs/hyper.pdf: $(MD_SOURCES)
	pandoc $(SHARED_PANDOC_OPTIONS) \
	-t latex \
	--latex-engine=xelatex \
	"--metadata=subtitle:$(VERSION)" \
	-o docs/hyper.pdf \
	$(MD_SOURCES)
