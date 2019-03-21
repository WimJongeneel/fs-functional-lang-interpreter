// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open System.IO

[<EntryPoint>]
let main argv =

  let code = File.ReadAllText argv.[0]

  let lexbuf = LexBuffer<char>.FromString code
  let ast: Expression list = Parser.start Lexer.tokenstream lexbuf
  evalExpressions ast


  0 