## Bugs
* [ ] Type checking for build-in functions
* [ ] Type aliases in REPL

## Typesystem

The language is strict and strongly typed. There is support for all the well-know primitives, literals, arrays, records and lambdas.

### Int
Ints contains a 32 bit integer value. There type is written als `int`. Ints also support literal typing. With literal typing we restrict the type to a specific value. A int literal is created by it's value (e.g `1` or `2`).
```hs
let i :: int = 1
let i1 :: 1 = 1
let i2 :: 2 = 2
```

### String
String contains a sequence of caracters. There type is written als `string`. They also support literal typing. You can get the length of a string with `count("string")`. A specific index is accesed with `.[ID]`.
```hs
let s :: string = "hello"
let s1 :: "x" = "x"
let s2 :: "hello" = "hello"
let s3 :: string = s2.[1]
let size :: int = count(s2)
```

### Bool
Bools contains true of false. They support literal typing.
```hs
let b :: bool = true
let b1 :: true = true
```

### Unit
Unit is used as the no-data type. It's type is written als `()`.
```hs
let u :: () = ()
```

### Arrays
Arrays contain multiple values of the same type. Their type is writting by post-fixing the inner type with `[]`. When constructing an array you pre-fix the array with the type (e.g `int [1]`). You can get the length again with `count(int[])`
```hs
let a :: int[] = int []
let a1 :: int[] = int [1, 2, 3]
let a2 :: 1[] = int [1, 1, 1]
let l :: int = count(a2)
```

### Records
Records contain key-value pairs. They are created with `{}`. You are allowed to have more properties in the actuval value then are requested by the type. However, the type system will in feature reference act like they aren't there.
```hs
let empty :: {} = {}
let o :: { x: 1 } = { x: 1 }
let o2 :: { x: 1, y: 3} = {x: 1, y: 3}
let o3 :: { y: 3 } = { x: 1, y: 3 }
```
You can acces properties with `obj.ID`.
```hs
let o2 :: { x: 1, y: 3} = {x: 1, y: 3}
let x :: 1 = o2.x
```
You can copy objects with an updated property like this:
```hs
let o2 :: { x: 1, y: int } = { x: 1, y: 3 }
let o3 :: { x: 1, y: int } = { o2 with y: 7 }
```

### Lambda
Lambda's are created with the `->`. When creating a lambda you always have to specify the type of the argument (`[PARAM] :: [TYPE] -> [EXPRESSION]`). There type is written with `[TYPE] -> [TYPE]`.
```hs
let f :: int -> int = x :: int -> x + 1
```
a lambda can return an other lambda. This means that multiple arguments and curring are supported. 
```hs
let add :: int -> int -> int = x :: int -> y :: int -> x + y
```
You can also pass a lambda as an argument to another lambda. For this you need `()` in the argument type:
```hs
let f :: (int -> int) -> int = g :: (int -> int) -> g(1)
```
Lambda's can have function body when using `{}`. The last expression is the return value. Lambda's get lexial scoping.
```hs
let f :: int -> int = x :: int -> {
  echo x
  x + 1
}
```

### Unions
Type unions are created by listing the case, seperated by the `|`. Conditionals of where the branches produce incopatible types will also be of a union of the 2 types.
```hs
let x :: int | bool = true
let y = true ? 1 : true
let z :: 1 | true = y
```

### Type aliases
Types can be given aliases to avoid repetition and extremely long expressions. They are created with `type [ID] = [TYPE]`.
```hs
type String = string
type compose = (int -> int) -> (int -> int) -> int -> int
let compose :: compose = f :: (int -> int) -> g :: (int -> int) -> x -> g(f(x))

type user = { 
  id: int,
  username: string 
}

let user :: user = {
  id: 1,
  username: "Dave"
}
```

### Notes on literals
Literal types are directly assignable to their corresponding primitive:
```hs
let x :: 1 = 1
let x1 :: int = x

let xx :: { x: 1 } = { x: 1}
let xx1 :: { x: int } = xx
```
Conditionals can be used to narrow a primitive to a literal type:
```hs
let x :: int = 1
let x1 :: 1 = x == 1 ? x : 1
```
Because both of the branches return something that is assignable to the type `1` we can assign the variable x (of type `int`) to the literal of type `1`.

### Type inference
Variables can also be declared without an type. They will get the type the typechecker infers from the expression assigned to them. Type inference will not work on parameters for functions.
```hs
let x = 1
let xx :: int = x