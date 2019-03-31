module Runtime

open System
open AST
open System.IO
open Microsoft.FSharp.Text.Lexing
open RuntimeFunctions
open TypeChecker

let rec evalExpression (mem: Memory<MemoryValue>) (expr: Expression) : Memory<MemoryValue> * MemoryValue = 
  match expr with
  | Value v                 -> mem, v
  | Read id                 -> mem, readMemory mem id
  | Write (id, e, _rec, _)  -> if Functions.ContainsKey id then Exception <| id + " is a reserved keyword" |> raise else
                               let m1, v1 = evalExpression mem e
                               let v2 = match (v1, _rec) with
                                        | (LambdaExpr (s, e, p, r), true) -> LambdaExpr (s, e, p, Some id)
                                        | _ -> v1
                               let m2 = writeMemory m1 id v2
                               m2, Unit ()
  | Lambda (p, es)          -> mem, LambdaExpr (mem, es, p, None)
  | Apply (e, p)            -> match e with
                               | Read i when Functions.ContainsKey i -> let (m1, p1) = evalExpression mem p
                                                                        Functions.[i] mem p1
                               | _ -> let (m1, l) = evalExpression mem e
                                      let (m2, p1) = evalExpression m1 p
                                      let res = match l with
                                                | LambdaExpr (s, es, p, r) -> applyLamda s es p1 p r
                                                | _ -> Exception "can not apply" |> raise
                                      m2, res
  | Nested e                -> evalExpression mem e
  | Echo e                  -> let (_, v) = evalExpression mem e
                               printf "%A\n" v
                               mem, Unit ()
  | Equals (l,r)            -> evalEquals mem l r
  | NotEquals (l,r)         -> evalNotEquals mem l r
  | Plus (l, r)             -> evalPlus mem l r
  | Min (l, r)              -> evalMin mem l r
  | Times (l, r)            -> evalTimes mem l r
  | Divide (l, r)           -> evalDivide mem l r
  | And (l, r)              -> evalAnd mem l r
  | Or (l, r)               -> evalOr mem l r
  | Condition (c, i, e)     -> let m1, will = evalExpression mem c
                               match will with
                                | Bool true -> evalExpression m1 i
                                | Bool false -> evalExpression m1 e
                                | _ -> Exception "Expected bool for if" |> raise
  | ArrayInit (l)           -> let list = List.map (evalExpression mem >> snd) l
                               (mem, Array.ofList list |> Array)
  | ArrayGet (e, i)         -> let m1, list = evalExpression mem e
                               let m2, index = evalExpression m1 i
                               match (list, index) with
                               | (Array l, Int i)       -> m1, if i < l.Length then l.[i] else Unit ()
                               | (String s, Int i)      -> m1, if i < s.Length then s.[i] |> string |> String else Unit ()
                               | _                      -> Exception "can not index the value" |> raise
  | ObjectInit r            -> mem, Map.ofList r |> Map.map (fun k e -> evalExpression mem e |> snd) |>  Object
  | ObjectGet (o, k)        -> let m1, obj = evalExpression mem o
                               match obj with
                               | Object r -> m1, r.[k]
                               | _                      -> Exception "can not index the value" |> raise
  | ObjectCopyWith (o, nv)  -> let m1, obj = evalExpression mem o
                               let m2, v = evalExpression m1 (snd nv)
                               match obj with
                               | Object r -> m2, Map.add (fst nv) v r |> Object
                               | _                      -> Exception "can not index the value" |> raise
  | Open s                  -> loadModule mem s

and loadModule (mem: Memory<MemoryValue>) (name: string): Memory<MemoryValue> * MemoryValue =
  let code = File.ReadAllText <| "./StandaardLib/" + name
  let ast: Expression list = Parser.start Lexer.tokenstream <| LexBuffer<char>.FromString code
  let mutable m1: Memory<MemoryValue> = mem
  List.map (fun e -> (let m2, _ = evalExpression m1 e
                    m1 <- m2;)) ast |> ignore
  m1, Unit ()

and evalAnd (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  match l1 with
  | Bool false -> m1, Bool false
  | Bool true -> let m2, r1 = evalExpression m1 rigth
                 match r1 with
                 | Bool _ -> m2, r1
                 | _ -> Exception "Type error with &&" |> raise
  | _ -> Exception "Type error with &&" |> raise

and evalOr (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Bool true, Bool _) -> m2, Bool true
  | (Bool _, Bool true) -> m2, Bool true
  | (Bool false, Bool false) -> m2, Bool false
  | _ -> Exception "Type error with ||" |> raise

and evalEquals (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)         -> m2, Bool (i1 = i2)
  | (Bool b1, Bool b2 )      -> m2, Bool (b1 = b2)
  | (Unit _, Unit _)         -> m2, Bool true
  | (String s1, String s2)   -> m2, Bool (s1 = s2)
  | _                        -> m2, Bool false

and evalNotEquals (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)         -> m2, Bool (i1 <> i2)
  | (Bool b1, Bool b2 )      -> m2, Bool (b1 <> b2)
  | (String s1, String s2)   -> m2, Bool (s1 <> s2)
  | (Unit _, Unit _)         -> m2, Bool false
  | _                        -> Exception "Type error with !=" |> raise

and evalPlus (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)        -> m2, Int (i1 + i2)
  | (String s1, String s2)  -> m2, String (s1 + s2)
  | _                    -> Exception "Type error with +" |> raise

and evalMin (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 - i2)
  | _                    -> Exception "Type error with -" |> raise

and evalTimes (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 * i2)
  | _                    -> Exception "Type error with *" |> raise

and evalDivide (mem: Memory<MemoryValue>) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 / i2)
  | _                    -> Exception "Type error with /" |> raise

and applyLamda (scope: Memory<MemoryValue>) (exprs: Expression list) (param: MemoryValue) (paramAlias: string ) (_rec: string option) =
  let mutable res = Unit ()
  let mutable mem = Map.empty :: scope
  if _rec.IsSome then mem <- writeMemory mem _rec.Value <| LambdaExpr (scope, exprs, paramAlias, _rec)
  mem <- writeMemory mem paramAlias param
  List.map (fun e -> (let m1, v = evalExpression mem e
                    mem <- m1;
                    res <- v;)) exprs |> ignore
  res

let evalExpressions (exprs: Expression list) = 
  typeCheckExpressions exprs

  let mutable mem: Memory<MemoryValue> = [Map.empty]
  List.map (fun e -> (let m1, _ = evalExpression mem e
                    mem <- m1;)) exprs |> ignore