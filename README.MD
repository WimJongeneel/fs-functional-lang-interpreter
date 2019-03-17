# Interpreter for a functional language

## Lambda
Lambda's are the main feature in this language. They are constructed with `->`. for example:
```
let id = x -> x
```
A lambda can contain multiple expressions if you place them between `{` and `}`. The last expresion of the lambda is the return value.
```
x -> {
  let y = 2
  echo x
  x == y
}
```
It is posible to pass lambda's as parameters and return them as values:
```
let compose = f -> g -> a -> g(f(a))
```