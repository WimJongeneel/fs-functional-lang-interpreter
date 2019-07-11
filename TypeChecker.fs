module TypeChecker

open System
open AST

type TypeEntry =
  | UnitType
  | IntType       of int option
  | BoolType      of bool option
  | StringType    of string option
  | FunctionType  of param: TypeEntry * ret: TypeEntry
  | ArrayType     of TypeEntry
  | ObjectType    of Map<string, TypeEntry>
  | UnionType     of TypeEntry list
  | GenericType   of id: string * restrictions: List<TypeEntry>

type TypeCheckerState = {
  vals: Memory<TypeEntry>
  types: Memory<TypeEntry>
}

let rec typeCheckExpression (expr: Expression) (s0: TypeCheckerState): TypeCheckerState * TypeEntry =
  match expr with
  | Read id                 -> s0, readMemory s0.vals id
  | Write (id, expr, _, t)  -> let s1, t1 = typeCheckExpression expr s0
                               if t.IsNone
                                then
                                  let m1 = writeMemory s1.vals id t1
                                  {s1 with vals = m1}, UnitType
                                else
                                  let rt = typeToTypeEntry s0.types t.Value
                                  let m2 = writeMemory s1.vals id rt
                                  match isAssignable rt t1 with
                                  | true   -> {s1 with vals = m2}, UnitType
                                  | false  -> Exception <| sprintf "Type error in let '%s', cannot assign '%A' to '%A'" id t1 rt |> raise
  | Value v                 -> s0, match v with
                                    | Int i     -> Some i |> IntType
                                    | Bool b    -> Some b |> BoolType
                                    | String s  -> Some s |> StringType
                                    | Unit _    -> UnitType
                                    | _         -> UnitType
  | Plus (l, r)             -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> s2, IntType None
                               | (StringType _, StringType _)   -> s2, StringType None
                               | _                              -> Exception "type error with Plus" |> raise
  | Min (l, r)              -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> s2, IntType None
                               | _                              -> Exception "type error with Min" |> raise
  | Times (l, r)            -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> s2, IntType None
                               | _                              -> Exception "type error with Times" |> raise
  | Divide (l, r)           -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> s2, IntType None
                               | _                              -> Exception "type error with Divide" |> raise
  | Echo e                  -> let m1, _ = typeCheckExpression e s0
                               m1, UnitType
  | And (l, r)              -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> s2, BoolType None
                               | _                        -> Exception <| sprintf "type error with '%A' && '%A' " l r |> raise
  | Or (l, r)               -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> s2, BoolType None
                               | _                        -> Exception "type error with Or" |> raise
  | Condition (p, t, f)     -> let ifTrueMemState = narrowIfScope s0 p
                               let ifS, ifTrueType = typeCheckExpression t {s0 with vals = ifTrueMemState}
                               let ifFalseMemState = ifTrueMemState.Tail  // should this not just be s0.vals?
                               let m2, ifFalseType = typeCheckExpression f {s0 with vals = ifFalseMemState}
                               if isAssignable ifTrueType ifFalseType
                                 then m2, ifTrueType
                                 elif isAssignable ifFalseType ifTrueType
                                 then m2, ifFalseType
                                 else m2, UnionType [ifTrueType; ifFalseType]
  | Nested e                -> typeCheckExpression e s0
  | Equals (l ,r)           -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (BoolType _, BoolType _)       -> s2, BoolType None
                               | (StringType _, StringType _)   -> s2, BoolType None
                               | (IntType _, IntType _)         -> s2, BoolType None
                               | (UnitType _, UnitType _)       -> s2, BoolType None
                               | _                              -> Exception <| sprintf "Type error with '%A' == '%A'" l r |> raise
  | NotEquals (l, r)        -> let s1, l1 = typeCheckExpression l s0
                               let s2, r1 = typeCheckExpression r s1
                               match (l1, r1) with
                               | (BoolType _, BoolType _)       -> s2, BoolType None
                               | (StringType _, StringType _)   -> s2, BoolType None
                               | (IntType _, IntType _)         -> s2, BoolType None
                               | (UnitType _, UnitType _)       -> s2, BoolType None
                               | _                              -> Exception <| sprintf "Type error with '%A' != '%A'" l r |> raise
  // For now disable lamda typeChecking
  // | Lambda (p, t, eb)          -> mem, FunctionType (typeToTypeEntry types t, typeCheckFuncBody types mem eb p <| typeToTypeEntry types t)
  | Apply (e, pe)           -> let s1, funcType = typeCheckExpression e s0  // this needs to updated to resolve the return type based on the generic type arguments
                               let s2, pt = typeCheckExpression pe s1
                               match funcType with
                               | FunctionType (p, r) when isAssignable p pt -> s2, r
                               | FunctionType (p, _)                        -> Exception <| sprintf "Given value '%A' is not assignable to param '%A'" pt p |> raise
                               | _                                          -> Exception <| sprintf "No not callable:  '%A'" e |> raise
  | ArrayInit (i, t)        -> let t1 = typeToTypeEntry s0.types t
                               List.map (fun e -> (let _, t2 = typeCheckExpression e s0;
                                                  if isAssignable t1 t2 |> not
                                                  then Exception <| sprintf "cannot assign '%A' to '%A'" t1 t |> raise)) i
                                                  |> ignore
                               s0, ArrayType t1
  | ArrayGet (a, i)         -> let s1, a1 = typeCheckExpression a s0
                               let m2, i1 = typeCheckExpression i s1
                               match (a1, i1) with
                               | (ArrayType t, IntType _) -> m2, UnionType [t; UnitType]
                               | (StringType _, IntType _) -> m2, UnionType [StringType None; UnitType]
                               | _                        -> Exception "Cannot index '%A' with '%A'" |> raise
  | ObjectInit e            -> let exprs = Map.ofList e
                               s0, ObjectType <| Map.map (fun id e -> (let _, t = typeCheckExpression e s0;
                                                                      t)) exprs
  | ObjectGet (ob, id)      -> let s1, obj = typeCheckExpression ob s0
                               match obj with
                               | ObjectType ps -> s1, ps.[id]
                               | _             -> Exception <| sprintf "'%A' is not an object" ob |> raise
  | ObjectCopyWith (ob, p)  -> let s1, obj = typeCheckExpression ob s0
                               let id, newVal = p
                               let s2, valT = typeCheckExpression newVal s1
                               match obj with
                               | ObjectType ps when isAssignable ps.[id] valT -> s2, obj
                               | _                                            -> Exception <| sprintf "cannot assign prop '%s' on '%A' with '%A'" id obj valT |> raise
  | TypeAlias _             -> s0, UnitType
  | _                       -> Exception <| sprintf "No type for %A" expr |> raise

and isAssignable (expected: TypeEntry) (given: TypeEntry) =
  match (expected, given) with
  | (StringType s1, StringType s2) when s1.IsSome && s2.IsSome -> s1.Value = s2.Value
  | (StringType s1, StringType _) when s1.IsNone              -> true
  | (IntType i1, IntType i2) when i1.IsSome && i2.IsSome       -> i1.Value = i2.Value
  | (IntType i1, IntType _) when i1.IsNone                    -> true
  | (BoolType b1, BoolType b2) when b1.IsSome && b2.IsSome     -> b1.Value = b2.Value
  | (BoolType b1, BoolType _) when b1.IsNone                  -> true
  | (UnitType, UnitType)                                      -> true
  | (FunctionType (p1, r1), FunctionType(p2, r2))             -> isAssignable p2 p1 && isAssignable r1 r2
  | (ArrayType t1, ArrayType t2)                              -> isAssignable t1 t2
  | (ObjectType o1, ObjectType o2)                            -> let unmatched = Map.tryPick (fun id t1 -> (
                                                                                                if o2.ContainsKey id && isAssignable t1 o2.[id]
                                                                                                then None
                                                                                                else Some t1
                                                                                              )) o1
                                                                 unmatched.IsNone
  | (_, UnionType cs2)                                        -> List.forall (fun c2 -> isAssignable expected c2) cs2
  | (UnionType cs, _)                                         -> List.exists (fun c -> isAssignable c given) cs
  | _                                                         -> false

and narrowIfScope (s0: TypeCheckerState) (pred: Expression) : Memory<TypeEntry> =
  match pred with
  | Equals (l, r) -> match (l, r) with
                     | (Value _, Read id) -> let _, t = typeCheckExpression l s0
                                             writeMemory s0.vals id t
                     | (Read id, Value _) -> let _, t = typeCheckExpression r s0
                                             writeMemory s0.vals id t
                     | _                  -> s0.vals
  | And (l, r)    -> let vals1 = narrowIfScope s0 l
                     narrowIfScope { s0 with vals = vals1 } r
  | Nested e      -> narrowIfScope s0 e
  | _             -> s0.vals

and typeToTypeEntry (types: Memory<TypeEntry>) (t: Type): TypeEntry =
  match t with
  | LiteralType lt      -> match lt with
                           | StringLiteral s -> StringType  <| Some s
                           | IntLiteral i    -> IntType     <| Some i
                           | BoolLiteral b   -> BoolType    <| Some b
                           | UnitLiteral     -> UnitType
  | NamedType nt        -> match nt with
                           | "string"        -> StringType None
                           | "int"           -> IntType None
                           | "bool"          -> BoolType None
                           | _               -> readMemory types nt
  | FuncType (p, r)     -> FunctionType (typeToTypeEntry types p, typeToTypeEntry types r)
  | NestedType t        -> typeToTypeEntry types t
  | Type.ArrayType t    -> ArrayType <| typeToTypeEntry types t
  | Type.ObjectType p   -> Map.map (fun _ t -> typeToTypeEntry types t) <| Map.ofList p |> ObjectType
  | Type.UnionType cs   -> List.map (fun c -> typeToTypeEntry types c) cs |> UnionType

and typeCheckFuncBody (s0: TypeCheckerState) (exprs: Expression list) (paramAlias: string) (param: TypeEntry) (* (generics: string*TypeEntry list) *): TypeEntry =
  let mutable m1: Memory<TypeEntry> = Map.empty :: s0.vals
  // let types = generics :: s0.types 
  let mutable t: TypeEntry = UnitType
  m1 <- writeMemory m1 paramAlias param
  List.map (fun e -> (let s1, t1 = typeCheckExpression e {s0 with vals = m1 } (* { types: types; vals = m1 } *)
                    m1 <- s1.vals;
                    t <- t1)) exprs |> ignore
  t

let typeAliasses (exprs: Expression list) : Memory<TypeEntry> =
  List.collect (fun e -> (
    match e with
    | TypeAlias (id, t) -> [(id, typeToTypeEntry [Map.empty] t)]
    | _                 -> []
  )) exprs
  |> Map.ofList
  |> fun m -> [m]

let typeCheckExpressions (exprs: Expression list) =
  let types = typeAliasses exprs
  let mutable s: TypeCheckerState = {types = types; vals = [Map.empty]}
  List.map (fun e -> (let s1, _ = typeCheckExpression e s
                    s <- s1;)) exprs |> ignore
