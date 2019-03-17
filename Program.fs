// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System

[<EntryPoint>]
let main argv =

  let test = """
  let f = n -> n == 1 ? 0 : n == 2 ? 10 : 20
  let x = true ? 1 : 0
  echo true ? 1 : 0
  """

  let lexbuf = LexBuffer<char>.FromString test
  let ast: Expression list = Parser.start Lexer.tokenstream lexbuf

  printf "%A \n\n" ast

  evalExpressions ast


  0 