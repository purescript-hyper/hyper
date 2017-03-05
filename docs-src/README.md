# Hyper Documentation

The Hyper documentation is built using Sphinx and reStructured Text.

## Prerequisites

* [A TeX Distribution](https://www.latex-project.org/get/)
* [virtualenv](https://virtualenv.pypa.io/en/stable/)

## Setup

```bash
# Change to this directory in your terminal
cd docs-src
virtualenv ENV
ENV/bin/activate
pip install -r requirements.txt
```

## Building HTML

```bash
make html
```

Or, a handy live-reloading server for when you are writing documentation:

```bash
make livehtml
```

## Building the PDF

```bash
make latexpdf
```
