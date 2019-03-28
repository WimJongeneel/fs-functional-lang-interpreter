module TypeChecker

open System
open AST

type TypeEntry = 
  | UnitType
  | IntType
  | BoolType
  | StringType
  | FunctionType of param: TypeEntry * ret: TypeEntry


let rec typeCheckExpression (mem: Memory<TypeEntry>) (expr: Expression): Memory<TypeEntry> * TypeEntry =
  match expr with
  | Read id                 -> mem, readMemory mem id
  | Write (id, expr, _)     -> let m1, t1 = typeCheckExpression mem expr
                               let m2 = writeMemory m1 id t1
                               m2, UnitType
  | Value v                 -> mem, match v with
                                    | Int _     -> IntType
                                    | Bool _    -> BoolType
                                    | String _  -> StringType
                                    | Unit _    -> UnitType
                                    | _         -> UnitType
  | Plus (l, r)             -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType, IntType)         -> m2, IntType
                               | (StringType, StringType)   -> m2, StringType
                               | _                          -> Exception "type error with Plus" |> raise
  | Min (l, r)              -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType, IntType)         -> m2, IntType
                               | (StringType, StringType)   -> m2, StringType
                               | _                          -> Exception "type error with Min" |> raise
  | Times (l, r)            -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType, IntType)         -> m2, IntType
                               | (StringType, StringType)   -> m2, StringType
                               | _                          -> Exception "type error with Times" |> raise
  | Divide (l, r)           -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (IntType, IntType)         -> m2, IntType
                               | (StringType, StringType)   -> m2, StringType
                               | _                          -> Exception "type error with Divide" |> raise
  | Echo e                  -> let m1, _ = typeCheckExpression mem e
                               m1, UnitType
  | And (l, r)              -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (BoolType, BoolType) -> m2, BoolType
                               | _                    -> Exception "type error with And" |> raise
  | Or (l, r)               -> let m1, l1 = typeCheckExpression mem l
                               let m2, r1 = typeCheckExpression m1 r
                               match (l1, r1) with
                               | (BoolType, BoolType) -> m2, BoolType
                               | _                    -> Exception "type error with Or" |> raise

let typeCheckExpressions (exprs: Expression list) = 
  let mutable mem: Memory<TypeEntry> = [Map.empty]
  List.map (fun e -> (let m1, _ = typeCheckExpression mem e
                    mem <- m1;)) exprs |> ignore