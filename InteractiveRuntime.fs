module InteractiveRuntime

open AST
open Runtime
open Microsoft.FSharp.Text.Lexing
open System.Text

let evalInput (mem: Memory) (code: string): Memory * MemoryValue = 
  let ast: Expression list = Parser.start Lexer.tokenstream <| LexBuffer<char>.FromString code
  let mutable m1: Memory = mem
  let mutable res = Unit ()
  List.map (fun e -> (let m2, r = evalExpression m1 e
                      m1 <- m2;
                      res <- r;
                      printfn "-- %A" r;)) ast |> ignore
  m1, res

let printWelcome () = 
  printf "-- REPL ready\n--\n"
  printf "-- commands:\n"
  printf "--   type ':mem' to inspect the runtimes memory\n"
  printf "--   type ':store [ID]' to store the last result in memory \n"
  printf "--   type ':save [FILE]' to save your code in a file \n"
  printf "--   type ':close' to close the session \n"
  printf "--\n-- happy coding\n"

let loop () = 
  let mutable mem: Memory = [Map.empty]
  let mutable res = Unit ()
  let mutable running = true
  let code = StringBuilder ()

  printWelcome ()

  while running do
    let input = System.Console.ReadLine()
    if input = ":mem" 
      then 
        printfn "%A" mem
      elif input.StartsWith ":store "
      then
        let name = input.Substring 7
        mem <- writeMemory mem name res
        printfn "-- Unit ()"
      elif input = ":close"
      then
        running <- false
        printfn "-- closing the session"
      elif input.StartsWith ":save "
      then
        let name = input.Substring 6
        System.IO.File.WriteAllText("./repl/" + name, code.ToString())
        printfn "-- saved your input at ./repl/%s" name
      else 
        code.AppendLine(input) |> ignore
        let m1, r = evalInput mem input
        mem <- m1
        res <- r
  ()