module TypeChecker

open System
open AST
open TypeCheckerState
open TypeConverter
open GenericTypes
open Typeassignability

let rec typeCheckExpression (expr: Expression) (s0: TypeCheckerState): TypeCheckerState * TypeEntry =
  match expr with
  | Read id                 -> let t = readMemory s0.vals id
                               match t with
                               | GenericType (_, Some r)  -> s0, r
                               | _                        -> s0, t
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
  | Echo e                  -> let s1, _ = typeCheckExpression e s0
                               s1, UnitType
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
  | Lambda (p, t, gs, eb)   -> let generics = Map.map (fun id r -> GenericType (id, Option.map (typeToTypeEntry s0.types) r)) gs
                               let s1 = {s0 with types = generics :: s0.types}
                               let paramType = typeToTypeEntry s1.types t
                               let returnType = typeCheckFuncBody s1 eb p paramType generics
                               let genericTypeArguments = Map.map (fun _ t -> Option.map (typeToTypeEntry s1.types) t) gs
                               s0, FunctionType <| (typeToTypeEntry s1.types t, genericTypeArguments , returnType)
  | Apply (e, ga, pe)       -> let s1, funcType = let s1, ut = typeCheckExpression e s0 in s1, resolveGenerics ut s1.types
                               let providedTypeArgs = List.map (typeToTypeEntry s0.types) ga
                               match funcType with
                                | FunctionType (p, eg, r) -> Map.toList eg
                                                              |> List.mapi (fun i tpo -> (let _, tp = tpo;
                                                                                          if tp.IsSome && isAssignable tp.Value providedTypeArgs.[i]  |> not
                                                                                          then Exception <| sprintf "Given type '%A' is not assignable to type '%A'" providedTypeArgs.[i] tp.Value |> raise
                                                                                          else true)) |> ignore
                                                             let localTypeScope =  Map.toList eg
                                                                                    |> List.mapi (fun i t -> let id, _ = t in (id, ga.[i]))
                                                                                    |> Map.ofList
                                                                                    |> Map.map (fun _ t -> typeToTypeEntry s1.types t)
                                                             let s2 = { s1 with types = localTypeScope :: s0.types }
                                                             let s3, providedParamType = typeCheckExpression pe s2
                                                             let expectedParamType = resolveGenerics p s3.types
                                                             let returnType = resolveGenerics r s3.types
                                                             let s4 = { s3 with types = s3.types.Tail }
                                                             if isAssignable expectedParamType providedParamType
                                                              then s4, returnType
                                                              else Exception <| sprintf "fsdgGiven value '%A' is not assignable to param '%A'" providedParamType expectedParamType |> raise
                                | _                      -> Exception <| sprintf "No not callable:  '%A'" e |> raise
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

and typeCheckFuncBody (s0: TypeCheckerState) (exprs: Expression list) (paramAlias: string) (param: TypeEntry) (generics: Map<string,TypeEntry>): TypeEntry =
  let mutable m1: Memory<TypeEntry> = Map.empty :: s0.vals
  let types = generics :: s0.types
  let mutable t: TypeEntry = UnitType
  m1 <- writeMemory m1 paramAlias param
  List.map (fun e -> (let s1, t1 = typeCheckExpression e { types = types; vals = m1 }
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
