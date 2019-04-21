// Learn more about F# at http://fsharp.org
open Microsoft.FSharp.Text.Lexing
open AST
open Runtime
open System.IO
open InteractiveRuntime
open TypeChecker

[<EntryPoint>]
let main argv =

  printfn "%A" <| isAssignable (UnionType [TypeEntry.UnitType; TypeEntry.IntType None; BoolType None]) (UnionType [TypeEntry.IntType <| Some 2; BoolType None])
  
  if argv.[0] = "-i" then
    loop ()
  else
    let code = File.ReadAllText argv.[0]

    let lexbuf = LexBuffer<char>.FromString code
    let ast: Expression list = Parser.start Lexer.tokenstream lexbuf
    evalExpressions ast


  0 