// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System

[<EntryPoint>]
let main argv =

  let test = """
  echo 1 + 1 * 2 - 1
  """

  let lexbuf = LexBuffer<char>.FromString test
  let ast: Expression list = Parser.start Lexer.tokenstream lexbuf

  printf "%A \n\n" ast

  evalExpressions ast


  0 