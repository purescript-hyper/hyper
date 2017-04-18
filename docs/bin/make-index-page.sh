#!/bin/bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


print_row() {
  echo "
<tr>
  <td class="version">$1</td>
  <td class="formats">
    <a href=\"docs/$1/index.html\">HTML</a> |
    <a href=\"docs/$1/hyper.pdf\">PDF</a>
  </td>
</tr>
"
}

versions=$(aws s3 ls hyper.wickstrom.tech/docs/ | awk '{print $2}' | sed 's/\///' | $DIR/sort_by_semver.py)
latest=$(echo $versions | head -n 1)

echo "
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Hyper</title>

  <meta name='keywords' content='http web purescript middleware routing type-safe'>
  <meta name='description' content='Hyper is an experimental middleware architecture for HTTP servers written in PureScript.'>
  <meta http-equiv='X-UA-Compatible' content='IE=edge,chrome=1'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1'>
  <meta name='apple-mobile-web-app-capable' content='yes'>
  <link rel='shortcut icon' href='static/favicon.png'>

  <meta name='twitter:card' content='summary'>
  <meta name='twitter:creator' content='owickstrom'>
  <meta name='twitter:title' content='Hyper'>
  <meta name='twitter:url' content='http://hyper.wickstrom.tech'>
  <meta name='twitter:description' content='Hyper is an experimental middleware architecture for HTTP servers written in PureScript.'>
  <meta name='twitter:image:src' content='http://hyper.wickstrom.tech/static/icon-large.png'>

  <meta property='og:type' content='article' />
  <meta property='og:article:author' content='Oskar Wickström' />
  <meta property='og:article:section' content='Technology' />
  <meta property='og:article:keyword' content='http web purescript middleware routing type-safe' />
  <meta property='og:title' content='Hyper' />
  <meta property='og:description' content='Hyper is an experimental middleware architecture for HTTP servers written in PureScript.'>
  <meta property='og:type' content='article' />
  <meta property='og:url' content='http://hyper.wickstrom.tech' />
  <meta property='og:image' content='http://hyper.wickstrom.tech/static/icon-large.png' />

  <link rel='stylesheet' href='static/style.css'>
  <link rel='stylesheet' href='static/bootstrap/css/bootstrap.min.css'>
  <link rel='stylesheet' href='static/overrides.css'>
  <link href='https://fonts.googleapis.com/css?family=Fira+Mono|Fira+Sans:400,400i,600' rel='stylesheet'>
  <style>
  #index {
    max-width: 700px;
  }
  #index h1 {
    text-align: center;
    margin: 2em 0 1em;
  }
  #index .rubric {
    text-align: center;
    font-style: normal;
    margin: 0 0 3rem;
  }
  .versions tr:first-child .version {
  }
  .versions tr:first-child .version:after {
    content: ' Latest';
    text-transform: uppercase;
    color: #579E00;
    font-weight: bold;
    font-size: 80%;
    vertical-align: super;
    letter-spacing: .06em;
  }
  </style>
</head>
<body>
<div class="container" id="index">
  <div class="row">
    <div class="col-xs-12">
      <h1>
        <img src="static/hyper@2x.png"
              alt="Hyper"
              width="300">
      </h1>
      <p class="rubric">
      Type-safe, statically checked composition of HTTP servers
      </p>
      <p>
      Hyper is an experimental middleware architecture for HTTP servers written in
      PureScript. Its main focus is correctness and type-safety, using type-level
      information to enforce correct composition and abstraction for web servers.
      The Hyper project is also a breeding ground for higher-level web server
      constructs, which tend to fall under the “framework” category.
      </p>
      <h2>Documentation</h2>
      <p>
      Choose a version and a format of the documentation below, or just grab <a
      href="docs/${latest}/index.html">the latest</a>.
      </p>

      <table class=\"versions table table-striped\">
        <thead>
          <tr><th>Version</th><th>Formats</th></tr>
        </thead>
          <tbody>"

  for version in $versions; do
    print_row $version
  done

  echo "
        </tbody>
      </table>
    </div>
  </div>
</div>
<footer class="footer" role="contentinfo">
  <div class="footer-wrapper text-muted">
    <a href="https://github.com/owickstrom/hyper">Hyper on GitHub</a>
    &mdash;
    &copy; Copyright 2016-2017 Oskar Wickström
    &mdash;
    <span class="license">
      Licensed under
      <a href="https://raw.githubusercontent.com/owickstrom/hyper/master/LICENSE">MPL-2.0</a>
    </span>
  </div>
</footer>
<script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
    ga('create', 'UA-42197774-3', 'auto');
    ga('send', 'pageview');
  </script>
</body>
</html>"
