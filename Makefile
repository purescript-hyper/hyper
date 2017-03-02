VERSION=$(shell git rev-parse --short HEAD)

MD_SOURCES=docs/src/index.md \
					 docs/src/introduction.md \
					 docs/src/design.md \
					 docs/src/basics.md \
					 docs/src/type-level-routing.md \
					 docs/src/nodejs.md \
					 docs/src/testing.md \
					 docs/src/contributing.md

DOCS_PURS_SOURCES=$(shell find docs/src -name '*.purs')
SHARED_THEME_FILES=$(shell find docs/theme -d 1)

.PHONY: docs
docs: docs/index.html docs/hyper.pdf docs/sitemap.xml

docs/sitemap.xml: docs/theme/html/sitemap.xml.tmpl docs/index.html docs/hyper.pdf
	cat docs/theme/html/sitemap.xml.tmpl | sed "s/YYYY-MM-DD/$(shell date '+%Y-%m-%d')/" > docs/sitemap.xml

docs/index.html: $(MD_SOURCES) $(DOCS_PURS_SOURCES) $(SHARED_THEME_FILES) $(shell find docs/theme/html)
	pandoc $(SHARED_PANDOC_OPTIONS) \
		-t html5 \
		--standalone \
		-S \
		--toc \
		--top-level-division=chapter \
		--filter pandoc-include-code \
		--no-highlight \
		-o docs/index.html \
		--base-header-level=2 \
		-V version:$(VERSION) \
		-V url:http://hyper.wickstrom.tech \
		-V logo1x:theme/hyper@1x.png \
		-V logo2x:theme/hyper@2x.png \
		-V 'description:Hyper is an experimental middleware architecture for HTTP servers written in PureScript.' \
		-V source-code-url:http://github.com/owickstrom/hyper \
		-V author-url:https://wickstrom.tech \
		-V 'license:Mozilla Public License 2.0' \
		-V license-url:https://raw.githubusercontent.com/owickstrom/hyper/master/LICENSE \
		--template=docs/theme/html/template.html \
	$(MD_SOURCES)

docs/hyper.pdf: $(MD_SOURCES) $(DOCS_PURS_SOURCES) $(SHARED_THEME_FILES) $(shell find docs/theme/latex)
	pandoc $(SHARED_PANDOC_OPTIONS) \
	-t latex \
	--listings \
	--filter pandoc-include-code \
	-H docs/theme/latex/purescript-language.tex \
	-H docs/theme/latex/listings.tex \
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
	pulp build -I docs/src/basics
	pulp build -I docs/src/type-level-routing
	pulp build -I examples
