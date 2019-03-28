module RuntimeFunctions

open System.IO
open AST

let Functions: Map<string, Memory<MemoryValue> -> MemoryValue -> Memory<MemoryValue> * MemoryValue> = Map.ofArray [|
  ("read", fun mem p -> mem, System.Console.ReadLine() |> String)
  ("string", fun mem p -> mem, match p with
                                | String s -> p
                                | Int i    -> string i |> String
                                | Bool b   -> if b then "true" |> String else "false" |> String
                                | _        -> string p |> String)
  ("bool", fun mem p -> mem, match p with
                              | Bool b     -> p
                              | Int i      -> if i = 1 then Bool true elif i = 0 then Bool false else Unit ()
                              | String s   -> if s = "true" then Bool true elif s = "false" then Bool false else Unit ()
                              | _          -> Unit ())
  ("int", fun mem p -> mem, match p with
                              | Bool b     -> if b then Int 1 else Int 0
                              | Int i      -> p
                              | String s   -> int s |> Int
                              | _          -> Unit ())
  ("count", fun mem p -> mem, match p with
                              | Array a    -> a.Length |> Int
                              | String s   -> s.Length |> Int
                              | _          -> Unit ())
  ("type", fun mem p -> mem, match p with
                              | Array _       -> String "array"
                              | String _      -> String "string"
                              | Int _         -> String "int"
                              | LambdaExpr _  -> String "lambda"
                              | Object _      -> String "record"
                              | Bool _        -> String "bool"
                              | Unit _        -> String "unit")
  ("has_key", fun mem p -> mem, match p with
                                | Array a -> match (a.[0], a.[1]) with
                                              | (Object r, String s) -> Map.containsKey s r |> Bool
                                              | _                    -> Bool false
                                | _       -> Bool false)
  ("array_count", fun mem p -> mem, match p with
                                    | Array a -> Int a.Length
                                    | _ -> Unit ())
  ("file_read", fun mem p -> mem, match p with
                                   | String s  -> File.ReadAllText s |> String
                                   | _         -> Unit ())
  ("file_write", fun mem p -> mem, match p with
                                   | Array a  -> let path = match a.[0] with
                                                            | String s -> s 
                                                            | _ -> System.Exception "no path in file_write" |> raise
                                                 let text = match a.[1] with
                                                            | String s -> s 
                                                            | _ -> System.Exception "no text in file_write" |> raise
                                                 File.WriteAllText(path, text)
                                                 Bool true
                                   | _         -> Bool false)
  ("file_exists", fun mem p -> mem, match p with
                                    | String s  -> File.Exists(s) |> Bool
                                    | _         -> Unit ())
  ("dir_exists", fun mem p -> mem, match p with
                                    | String s  -> System.IO.Directory.Exists(s) |> Bool
                                    | _         -> Unit ())
  ("dir_create", fun mem p -> mem, match p with
                                    | String s  -> System.IO.Directory.CreateDirectory(s) |> ignore
                                                   Bool true
                                    | _         -> Unit ())
|]