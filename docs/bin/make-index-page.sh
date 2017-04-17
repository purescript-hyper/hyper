#!/bin/bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


print_row() {
  echo "
<tr>
  <td class="version">$1</td>
  <td class="formats">
    <a href=\"docs/$1/\">HTML</a> |
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
  <link rel="stylesheet" href="static/style.css">
  <link rel="stylesheet" href="static/fira/fira.css">
  <link rel="stylesheet" href="static/fira-code/fira_code.css">
  <link rel="stylesheet" href="static/bootstrap/css/bootstrap.min.css">
  <link rel="stylesheet" href="static/overrides.css">
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
    href="docs/${latest}/">the latest</a>.
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
</body>
</html>"
