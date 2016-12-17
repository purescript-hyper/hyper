# Example: Form Parser

From the root of this repository, start the server by running:

``` shell
$ pulp run -I examples/form-parser
```

Then, using `curl`, you can try it out:

``` shell
$ curl -d 'foobar' localhost:3000
Bad request, invalid form.
$ curl -d 'foo=bar' localhost:3000
[(Tuple "foo" "bar")]
```
