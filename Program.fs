// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System

[<EntryPoint>]
let main argv =

  let test = """
  let compose = f -> g -> a -> g(f(a))
let comp1 = compose(x -> 2)
let comp2 = comp1(x -> {
  echo x
  2
})
comp2(1)
  """

  let lexbuf = LexBuffer<char>.FromString test
  let ast: Expression list = Parser.start Lexer.tokenstream lexbuf

  printf "%A \n\n" ast

  evalExpressions ast


  0 