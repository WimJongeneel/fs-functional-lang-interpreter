module TypeChecker

open System
open AST

type TypeEntry = 
  | UnitType
  | IntType of int option
  | BoolType of bool option
  | StringType of string option
  | FunctionType of param: TypeEntry * ret: TypeEntry

let rec typeCheckExpression (mem: Memory<TypeEntry>) (expr: Expression): Memory<TypeEntry> * TypeEntry =
  match expr with
  | Read id                 -> mem, readMemory mem id
  | Write (id, expr, _, t)  -> let m1, t1 = typeCheckExpression mem expr
                               let rt = typeToTypeEntry t
                               let m2 = writeMemory m1 id rt
                               match isAssignable rt t1 with
                               | true   -> m2, UnitType
                               | false  -> Exception <| sprintf "Type error in let '%s', cannot assign '%A' to '%A'" id t1 rt |> raise
  | Value v                 -> mem, match v with
                                    | Int i     -> Some i |> IntType
                                    | Bool b    -> Some b |> BoolType
                                    | String s  -> Some s |> StringType
                                    | Unit _    -> UnitType
                                    | _         -> UnitType
  | Plus (l, r)             -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                          -> Exception "type error with Plus" |> raise
  | Min (l, r)              -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                          -> Exception "type error with Min" |> raise
  | Times (l, r)            -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                          -> Exception "type error with Times" |> raise
  | Divide (l, r)           -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                          -> Exception "type error with Divide" |> raise
  | Echo e                  -> let m1, _ = typeCheckExpression mem e
                               m1, UnitType
  | And (l, r)              -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> m2, BoolType None
                               | _                    -> Exception <| sprintf "type error with '%A' && '%A' " l r |> raise
  | Or (l, r)               -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> m2, BoolType None
                               | _                    -> Exception "type error with Or" |> raise
  | Condition (p, t, f)     -> let ifScope = narrowIfScope <| Map.empty :: mem <| p
                               let _, ifT = typeCheckExpression ifScope t
                               let m1 = ifScope.Tail
                               let m2, falseT = typeCheckExpression m1 f
                               if isAssignable ifT falseT
                                 then m2, ifT
                                 elif isAssignable falseT ifT
                                 then m2, falseT
                                 else  Exception <| sprintf "type error in ? :, '%A' is not compatable with %A " ifT falseT |> raise
  | Nested e                -> typeCheckExpression mem e
  | Equals (l ,r)           -> let m1, lt = typeCheckExpression mem l
                               let m2, rt = typeCheckExpression m1 r
                               match (lt, rt) with
                               | (BoolType _, BoolType _)       -> m2, BoolType None
                               | (StringType _, StringType _)   -> m2, BoolType None
                               | (IntType _, IntType _)         -> m2, BoolType None
                               | (UnitType _, UnitType _)       -> m2, BoolType None
                               | _                              -> Exception <| sprintf "Type error with '%A' == '%A'" l r |> raise
  | NotEquals (l, r)        -> let m1, lt = typeCheckExpression mem l
                               let m2, rt = typeCheckExpression m1 r
                               match (lt, rt) with
                               | (BoolType _, BoolType _)       -> m2, BoolType None
                               | (StringType _, StringType _)   -> m2, BoolType None
                               | (IntType _, IntType _)         -> m2, BoolType None
                               | (UnitType _, UnitType _)       -> m2, BoolType None
                               | _                              -> Exception <| sprintf "Type error with '%A' != '%A'" l r |> raise
  | Lambda (_, t, eb)          -> mem, FunctionType (typeToTypeEntry t, typeCheckFuncBody mem eb)
  | Apply (e, pe)           -> let m1, f = typeCheckExpression mem e
                               let m2, pt = typeCheckExpression m1 pe
                               match f with
                               | FunctionType (p, r) when isAssignable pt p -> m2, r
                               | FunctionType (p, _)                        -> Exception <| sprintf "Given value '%A' is not assignable to param '%A'" pt p |> raise
                               | _                                          -> Exception <| sprintf "No not callable:  '%A'" e |> raise
  | _                       -> Exception <| sprintf "No type for %A" expr |> raise

and isAssignable (expected: TypeEntry) (given: TypeEntry) = 
  match (expected, given) with
  | (StringType s1, StringType s2) when s1.IsSome && s2.IsSome -> s1.Value = s2.Value
  | (StringType s1, StringType _) when s1.IsNone              -> true
  | (IntType i1, IntType i2) when i1.IsSome && i2.IsSome       -> i1.Value = i2.Value
  | (IntType i1, IntType i2) when i1.IsNone                   -> true
  | (BoolType b1, BoolType b2) when b1.IsSome && b2.IsSome     -> b1.Value = b2.Value
  | (BoolType b1, BoolType _) when b1.IsNone                  -> true
  | (UnitType, UnitType)                                      -> true
  | (FunctionType (p1, r1), FunctionType(p2, r2))             -> isAssignable p2 p1 && isAssignable r1 r2
  | _                                                         -> false

and narrowIfScope (mem: Memory<TypeEntry>) (pred: Expression) : Memory<TypeEntry> = 
  match pred with
  | Equals (l, r) -> match (l, r) with
                     | (Value _, Read id) -> let _, t = typeCheckExpression mem l
                                             writeMemory mem id t
                     | (Read id, Value _) -> let _, t = typeCheckExpression mem r
                                             writeMemory mem id t
                     | _                  -> mem
  | And (l, r)    -> let m1 = narrowIfScope mem l
                     narrowIfScope m1 r
  | Nested e      -> narrowIfScope mem e
  | _             -> mem

and typeToTypeEntry (t: Type) =
  match t with
  | LiteralType lt      -> match lt with
                           | StringLiteral s -> StringType <| Some s
                           | IntLiteral i    -> IntType <| Some i
                           | BoolLiteral b   -> BoolType <| Some b
                           | UnitLiteral     -> UnitType
  | NamedType nt        -> match nt with
                           | "string" -> StringType None
                           | "int"    -> IntType None
                           | "bool"   -> BoolType None
                           | _        -> UnitType
  | FuncType (p, r)     -> FunctionType (typeToTypeEntry p, typeToTypeEntry r)

and typeCheckFuncBody (mem: Memory<TypeEntry>) (exprs: Expression list): TypeEntry = 
  let mutable m1: Memory<TypeEntry> = Map.empty :: mem
  let mutable t : TypeEntry = UnitType
  List.map (fun e -> (let m2, t1 = typeCheckExpression m1 e
                    m1 <- m2;
                    t <- t1)) exprs |> ignore
  t

let typeCheckExpressions (exprs: Expression list) = 
  let mutable mem: Memory<TypeEntry> = [Map.empty]
  List.map (fun e -> (let m1, _ = typeCheckExpression mem e
                    mem <- m1;)) exprs |> ignore