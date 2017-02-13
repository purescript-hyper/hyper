VERSION=$(shell git rev-parse --short HEAD)

MD_SOURCES=docs/index.md \
					 docs/goals.md \
					 docs/design.md \
					 docs/request-body-parsing.md \
					 docs/type-level-routing.md \
					 docs/servers.md \
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
		--no-highlight \
		-c docs.css \
		-o docs/index.html \
		--base-header-level=2 \
		-V version:$(VERSION) \
		-V url:https://owickstrom.github.io/hyper \
		-V logo1x:hyper@1x.png \
		-V logo2x:hyper@2x.png \
		-V source-code-url:https://github.com/owickstrom/hyper \
		-V author-url:https://wickstrom.tech \
		-V 'license:Mozilla Public License 2.0' \
		-V license-url:https://raw.githubusercontent.com/owickstrom/hyper/master/LICENSE \
		--template=docs/template.html \
	$(MD_SOURCES)

docs/hyper.pdf: $(MD_SOURCES) $(shell find docs -name '*.tex')
	pandoc $(SHARED_PANDOC_OPTIONS) \
	-t latex \
	--listings \
	-H docs/purescript-language.tex \
	-H docs/listings.tex \
	-V links-as-notes=true \
	-V documentclass=article \
	--toc --toc-depth=2 \
	 --number-sections \
	--latex-engine=xelatex \
	"--metadata=date:$(VERSION)" \
	-o docs/hyper.pdf \
	$(MD_SOURCES)

.PHONY: examples
examples:
	find examples/* -type d -exec rm -rf output/Main \; -exec pulp build -I {} \;
