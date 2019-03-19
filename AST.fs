module AST 
open System

type MemoryValue =
| Int of int
| Bool of bool
| LambdaExpr of scope: Memory * exprs: Expression list * param: string
| Unit of unit

and Expression = 
| Value of MemoryValue
| Read of id: string
| Write of id: string * expr: Expression
| Lambda of param: string * exprs: Expression list
| Apply of func: Expression * param: Expression
| Nested of Expression
| Echo of Expression
| Equals of left: Expression * rigth: Expression
| NotEquals of left: Expression * rigth: Expression
| Plus of left: Expression * rigth: Expression
| Min of left: Expression * rigth: Expression
| Times of left: Expression * rigth: Expression
| Divide of left: Expression * rigth: Expression
| Condition of con: Expression * _then: Expression * _else: Expression
| And of left: Expression * rigth: Expression
| Or of left: Expression * rigth: Expression
 

and Memory = List<Map<string, MemoryValue>>

let readMemory (mem: Memory) (id: string) =
  List.tryPick (fun scope -> if Map.containsKey id scope then scope.[id] |> Some else None) mem
  |> fun v -> if v.IsSome then v.Value else Exception <| "Variable not defined: " + id |> raise

let writeMemory (mem: Memory) (id: string) (v: MemoryValue) =
  let scope = mem.Head |> fun scope -> scope.Add (id, v)
  scope :: mem.Tail

let rec evalExpression (mem: Memory) (expr: Expression) : Memory * MemoryValue = 
  match expr with
  | Value v                 -> mem, v
  | Read id                 -> mem, readMemory mem id
  | Write (id, e)           -> let m1, v1 = evalExpression mem e
                               let m2 = writeMemory m1 id v1
                               m2, Unit ()
  | Lambda (p, es)          -> mem, LambdaExpr (mem, es, p)
  | Apply (e, p)            -> let (m1, l) = evalExpression mem e
                               let (m2, p1) = evalExpression m1 p
                               let res = match l with
                                          | LambdaExpr (s, es, p) -> applyLamda s es p1 p
                                          | _ -> Exception "can not apply" |> raise
                               m1, res
  | Nested e                -> evalExpression mem e
  | Echo e                  -> let (_, v) = evalExpression mem e
                               printf "%A" v
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

and evalAnd (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  match l1 with
  | Bool false -> m1, Bool false
  | Bool true -> let m2, r1 = evalExpression m1 rigth
                 match r1 with
                 | Bool _ -> m2, r1
                 | _ -> Exception "Type error with &&" |> raise
  | _ -> Exception "Type error with &&" |> raise

and evalOr (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Bool true, Bool _) -> m2, Bool true
  | (Bool _, Bool true) -> m2, Bool true
  | (Bool false, Bool false) -> m2, Bool false
  | _ -> Exception "Type error with ||" |> raise

and evalEquals (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Bool (i1 = i2)
  | (Bool b1, Bool b2 )  -> m2, Bool (b1 = b2)
  | (Unit _, Unit _)     -> m2, Bool true
  | _                    -> Exception "Type error with ==" |> raise

and evalNotEquals (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Bool (i1 <> i2)
  | (Bool b1, Bool b2 )  -> m2, Bool (b1 <> b2)
  | (Unit _, Unit _)     -> m2, Bool false
  | _                    -> Exception "Type error with !=" |> raise

and evalPlus (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 + i2)
  | _                    -> Exception "Type error with +" |> raise

and evalMin (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 - i2)
  | _                    -> Exception "Type error with -" |> raise

and evalTimes (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 * i2)
  | _                    -> Exception "Type error with *" |> raise

and evalDivide (mem: Memory) (left: Expression) (rigth: Expression) = 
  let m1, l1 = evalExpression mem left
  let m2, r1 = evalExpression m1 rigth
  match (l1, r1) with
  | (Int i1, Int i2)     -> m2, Int (i1 / i2)
  | _                    -> Exception "Type error with /" |> raise

and applyLamda (scope: Memory) (exprs: Expression list) (param: MemoryValue) (paramAlias: string )=
  let mutable res = Unit ()
  let mutable mem = Map.empty :: scope
  mem <- writeMemory mem paramAlias param
  List.map (fun e -> (let m1, v = evalExpression mem e
                    mem <- m1;
                    res <- v;)) exprs |> ignore
  res

let evalExpressions (exprs: Expression list) = 
  let mutable mem: Memory = [Map.empty]
  List.map (fun e -> (let m1, _ = evalExpression mem e
                    mem <- m1;)) exprs |> ignore