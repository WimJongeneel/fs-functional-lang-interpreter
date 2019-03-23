module RuntimeFunctions

open AST

let Functions: Map<string, Memory -> MemoryValue -> Memory * MemoryValue> = Map.ofArray [|
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
|]