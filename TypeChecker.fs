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
                               let m2 = writeMemory m1 id t1
                               let rt = match t with
                                        | LiteralType lt -> match lt with
                                                             | StringLiteral s -> StringType <| Some s
                                                             | IntLiteral i    -> IntType <| Some i
                                                             | BoolLiteral b   -> BoolType <| Some b
                                                             | UnitLiteral     -> UnitType
                                        | NamedType nt    -> match nt with
                                                             | "string" -> StringType None
                                                             | "int"    -> IntType None
                                                             | "bool"   -> BoolType None
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
                               | _                    -> Exception "type error with And" |> raise
  | Or (l, r)               -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> m2, BoolType None
                               | _                    -> Exception "type error with Or" |> raise

and isAssignable (expected: TypeEntry) (given: TypeEntry) = 
  match (expected, given) with
  | (StringType s1, StringType s2) when s1.IsSome && s2.IsSome -> s1.Value = s2.Value
  | (StringType s1, StringType _) when s1.IsNone              -> true
  | (IntType i1, IntType i2) when i1.IsSome && i2.IsSome       -> i1.Value = i2.Value
  | (IntType i1, IntType i2) when i1.IsNone                   -> true
  | (BoolType b1, BoolType b2) when b1.IsSome && b2.IsSome     -> b1.Value = b2.Value
  | (BoolType b1, BoolType _) when b1.IsNone                  -> true
  | (UnitType, UnitType)                                      -> true
  | _                                                         -> false

let typeCheckExpressions (exprs: Expression list) = 
  let mutable mem: Memory<TypeEntry> = [Map.empty]
  List.map (fun e -> (let m1, _ = typeCheckExpression mem e
                    mem <- m1;)) exprs |> ignore