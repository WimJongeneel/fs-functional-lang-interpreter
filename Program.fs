// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System

[<EntryPoint>]
let main argv =

  let test = """
  let eq = a -> b -> a == b
  echo eq(1)(2)
  let comp = f -> g -> a -> g(f(a))
  comp(x -> {x == true})(x -> {echo x})(false)
  """

  let lexbuf = LexBuffer<char>.FromString test
  let ast: Expression list = Parser.start Lexer.tokenstream lexbuf

  printf "%A \n\n" ast

  evalExpressions ast


  0 