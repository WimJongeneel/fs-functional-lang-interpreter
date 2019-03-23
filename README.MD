# Interpreter for a functional language

* [REPL](#repl)
* [Variables](#variables)
  * [Primitives](#primitives)
  * [Arrays](#arrays)
  * [Records](#records)
  * [Lambdas](#lambdas)
* [Operators](#operators)
  * [Math](#math)
  * [Comparison](#comparison)
* [Modules](#modules)
* [Conditionals](#Conditionals)
* [Echo](#echo)

## REPL
run the interperter with the `-i` flag to launchs the REPL. In here you can experiment with the langauge.
```
-- REPL ready
--
-- commands:
--   type ':mem' to inspect the runtimes memory
--   type ':store [ID]' to store the last result in memory 
--   type ':save [FILE]' to save your code in a file 
--   type ':close' to close the session 
--
-- happy coding
```

## Variables
Variables are created with the `let` keyword. For example:
```
let x = `
```
The language has lexial scoping. 

### Primitives
The different types of primitives are: `Int`, `Bool` and `String`.
```
let n = 1
let s = "string"
let b = true
```

### Arrays
Arrays are created with the `[` and `]`. It is posible to mix types within an array.
```
let ar = [1,2,3]
let mix = [1, true, "string"]
```
Items are accesed with the `.[INDEX]`.
```
let arr = [1,3]
let x = arr.[1]
``` 

### Records
Records are object witch store key-value pairs. The look as follows:
```
let r = {
  x: 1,
  xx: true,
  xxx: "string"
}
```
Properties can be accesed with `.ID`:
```
r.xx
```

### Lambdas
Lambdas are created with the `->`:
let f = x -> x + 1
```
Lambdas can contain multiple expresion with a codeblock. The last expression is the return value.
```
let f1 = x -> {
  echo x
  x + 1
}
```
Recursion is supported with the `rec` keyword:
let rec f = x -> f(1)
```

## Operators

### Math
The math operators are, in order of predence:
* `*`: `1 * 1`
* `/`: `1 / 1`
* `+`: `1 + 1`
* `-`: `1 - 1`

### Comparison
the comparison operators are `==` and `!=`

## Modules
Modules of the standard lib are loaded with `open [NAME]`. The modules are:
* List: list implementation in the language
* Option: optoin monad implementation in the language

## Conditionals
Conditionals are supported with the `prediction ? when-true : when-false`.
```
let bool_to_int = b -> b ? 1 : 0
```

## Echo
use `echo [EXPR]` to print the result of a expresion.
```
echo 1
echo 1 + 1
```