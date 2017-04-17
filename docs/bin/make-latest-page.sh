#!/bin/bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

version=$(aws s3 ls docs.enterprise.codescene.io/versions/ | awk '{print $2}' | sed 's/\///' | $DIR/sort_by_semver.py | head -n 1)
url="https://hyper.wickstrom.tech/docs/${version}/"

echo "
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Latest Release &mdash; Hyper</title>
</head>
<body>
<p>Redirecting to latest release of Hyper, at <a href="${url}">${url}</a>...</p>
<script type="text/javascript">
window.location = '${url}';
</script>
</body>
</html>"
