// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open Runtime
open System.IO
open InteractiveRuntime
open TypeChecker

[<EntryPoint>]
let main argv =

  if argv.[0] = "-i" then
    loop ()
  else
    let code = File.ReadAllText argv.[0]

    let lexbuf = LexBuffer<char>.FromString code
    let ast: Expression list = Parser.start Lexer.tokenstream lexbuf
    evalExpressions ast


  0 