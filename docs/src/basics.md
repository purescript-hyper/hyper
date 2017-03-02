# Basics

This chapter walks through some of the basic and more lower-level features of
Hyper. We look at how we can use the Conn and Middleware types to gain type
safety "in the small", and how to implement common things needed in web servers.

## Request Body Reading

The `RequestBodyReader` type class has one operation, `readBody`, which supports
different servers to provide different types of request body values.

```purescript
class RequestBodyReader r m b | r -> b where
  readBody
    :: forall req res c.
       Middleware
       m
       (Conn { body :: r | req } res c)
       (Conn { body :: r | req } res c)
       b
```

Given that there is an instance for the body `b`, and the return type `r`, we
can use this middleware together with other middleware, like so:

```{.purescript include=docs/src/basics/ReadBody.purs snippet=onPost}
```

## Forms

When working with form data, we often want to serialize and deserialize forms as
custom data types, instead of working with the key-value pairs directly. The
`ToForm` and `FromForm` type classes abstracts serialization and deserialization
to form data, respectively.

We first declare our data types, and some instance which we will need later.

```{.purescript include=docs/src/basics/FormSerialization.purs snippet=datatypes}
```

In this example we will only deserialize forms, and thus we only need the
`FromForm` instance.

```{.purescript include=docs/src/basics/FormSerialization.purs snippet=parsing}
```

Now we are ready to write our handler. We use `parseFromForm` to get a value
of type `Either String Order`, where the `String` explains parsing errors. By
pattern matching using record field puns, we extract the `beers` and `meal`
values, and respond based on those values.

```{.purescript include=docs/src/basics/FormSerialization.purs snippet=onPost}
```
