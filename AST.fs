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

and Memory = List<Map<string, MemoryValue>>

let readMemory (mem: Memory) (id: string) =
  List.tryPick (fun scope -> if Map.containsKey id scope then scope.[id] |> Some else None) mem
  |> fun v -> if v.IsSome then v.Value else Exception <| "Variable not defined: " + id |> raise

let writeMemory (mem: Memory) (id: string) (v: MemoryValue) =
  let scope = mem.Head |> fun scope -> scope.Add (id, v)
  scope :: mem.Tail

let rec evalExpression (mem: Memory) (expr: Expression) : Memory * MemoryValue = 
  match expr with
  | Value v         -> mem, v
  | Read id         -> mem, readMemory mem id
  | Write (id, e)   -> let m1, v1 = evalExpression mem e
                       let m2 = writeMemory m1 id v1
                       m2, Unit ()
  | Lambda (p, es)  -> mem, LambdaExpr (mem, es, p)
  | Apply (e, p)    -> let (m1, l) = evalExpression mem e
                       let (m2, p1) = evalExpression m1 p
                       let res = match l with
                                  | LambdaExpr (s, es, p) -> applyLamda s es p1 p
                                  | _ -> Exception "can not apply" |> raise
                       m1, res
  | Nested e        -> evalExpression mem e
  | Echo e          -> let (_, v) = evalExpression mem e
                       printf "%A" v
                       mem, Unit ()
  | Equals (l,r)    -> let m1, l1 = evalExpression mem l
                       let m2, r1 = evalExpression m1 r
                       match (l1, r1) with
                       | (Int i1, Int i2)     -> m2, Bool (i1 = i2)
                       | (Bool b1, Bool b2 )  -> m2, Bool (b1 = b2)
                       | (Unit _, Unit _)         -> m2, Bool true
                       | _ -> Exception "Type error with ==" |> raise

and applyLamda (scope: Memory) (exprs: Expression list) (param: MemoryValue) (paramAlias: string )=
  let mutable res = Unit ()
  let mutable mem = Map.empty :: scope
  mem <- writeMemory mem paramAlias param
  List.map (fun e -> (let m1, v = evalExpression mem e
                    mem <- m1;
                    res <- v;)) exprs |> ignore
  res

and evalExpressions (exprs: Expression list) = 
  let mutable mem: Memory = [Map.empty]
  List.map (fun e -> (let m1, _ = evalExpression mem e
                    mem <- m1;)) exprs |> ignore