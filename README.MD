# Interpreter for a functional language

## Lambda
Lambda's are the main feature in this language. They are constructed with `->`. for example:
```
let id = x -> x
```
A lambda can contain multiple expressions if you place them between `{` and `}`. The last expresion of the lambda is the return value.
```
let myfun = x -> {
  let y = 2
  echo x
  x == y
}
```
It is posible to pass lambda's as parameters and return them as values. This means partial application (or curring) is supported out of the box.
```
let compose = f -> g -> a -> g(f(a))
let comp1 = compose(x -> 2)
let comp2 = comp1(x -> {
  echo x
  2
})
comp2(1)
```
Every lambda does have it's own lexial scope.

## Variables
The language supports ints and booleans and unit (outside lambda's). They are constructed as follows:
```
let int = 1
let bool = false
let bool1 = true
let u = ()
```
Type's don't implicit cast on runtime.

## Lists
Collections are implemented using lists. They are constructed with the `[` and `]`:
```
let list = [1, 2, 3, 4]
```
Items are accessed with `/[*]`:
```
let list = [1,2,3]
echo list
echo list.[1]
```

## Records
Records store key-value pairs. They are constructed as follows:
```
let r = {
  x: 1,
  xx: 2
}
```
Accessing properties happens with the `.`:
```
echo r.x
```
New records can be based on existing records by using `with`:
```
let r = {
  x: 1
}
let r2 = { r with xx: 2 }
echo r2.x
```

## Operators
The language supports the following math operators, in order of predence:
* times: `*`
* divide: `/`
* add: `+`
* min: `-`

## Conditional
Controlflow is provided with the `conditional ? when-true : when-false`.
```
let boolToInt = b -> b ? 1 : 0
```
It is posible to put more conditionals in one of the branches.
```
let x = n -> n == 1 ? 10 : n == 2 ? 20 : 30
```

## Echo
The `echo` keyword lets you print any variable.
```
echo 1
```