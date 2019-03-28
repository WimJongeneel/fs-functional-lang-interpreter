module AST

open System
open System.Linq.Expressions

type MemoryValue =
| String of string
| Int of int
| Bool of bool
| LambdaExpr of scope: Memory<MemoryValue> * exprs: Expression list * param: string * _rec: string option
| Unit of unit
| Array of MemoryValue []
| Object of Map<string, MemoryValue>

and Expression = 
| Value of MemoryValue
| Read of id: string
| Write of id: string * expr: Expression * _rec: bool
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
| ArrayInit of Expression list
| ArrayGet of list: Expression * index: Expression
| ObjectInit of (string * Expression) list
| ObjectGet of obj: Expression * key: string
| ObjectCopyWith of obj: Expression * newValue: (string * Expression)
| Open of string

and Memory<'m> = List<Map<string, 'm>>

let readMemory<'m> (mem: Memory<'m>) (id: string) =
  List.tryPick (fun scope -> if Map.containsKey id scope then scope.[id] |> Some else None) mem
  |> fun v -> if v.IsSome then v.Value else Exception <| "Variable not defined: " + id |> raise

let writeMemory<'m> (mem: Memory<'m>) (id: string) (v: 'm) =
  let scope = mem.Head |> fun scope -> scope.Add (id, v)
  scope :: mem.Tail