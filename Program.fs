// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System.IO

[<EntryPoint>]
let main argv =

  let code = File.ReadAllText argv.[0]

  let test = """
  let f = n -> n == 1 ? 0 : n == 2 ? 10 : 20
  echo f(3) - f(2)
  echo true ? 1 : 0
  """

  let lexbuf = LexBuffer<char>.FromString code
  let ast: Expression list = Parser.start Lexer.tokenstream lexbuf
  evalExpressions ast


  0 