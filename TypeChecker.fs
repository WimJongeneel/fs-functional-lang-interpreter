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

let rec typeCheckExpression (types: Map<string, TypeEntry>) (mem: Memory<TypeEntry>) (expr: Expression): Memory<TypeEntry> * TypeEntry =
  match expr with
  | Read id                 -> mem, readMemory mem id
  | Write (id, expr, _, t)  -> let m1, t1 = typeCheckExpression types mem expr
                               let rt = typeToTypeEntry types t
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
  | Plus (l, r)             -> let m1, l1 = typeCheckExpression types mem l
                               let m2, r1 = typeCheckExpression types m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                              -> Exception "type error with Plus" |> raise
  | Min (l, r)              -> let m1, l1 = typeCheckExpression types mem l
                               let m2, r1 = typeCheckExpression types m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                              -> Exception "type error with Min" |> raise
  | Times (l, r)            -> let m1, l1 = typeCheckExpression types mem l
                               let m2, r1 = typeCheckExpression types m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                              -> Exception "type error with Times" |> raise
  | Divide (l, r)           -> let m1, l1 = typeCheckExpression types mem l
                               let m2, r1 = typeCheckExpression types m1 r
                               match (l1, r1) with
                               | (IntType _, IntType _)         -> m2, IntType None
                               | (StringType _, StringType _)   -> m2, StringType None
                               | _                              -> Exception "type error with Divide" |> raise
  | Echo e                  -> let m1, _ = typeCheckExpression types mem e
                               m1, UnitType
  | And (l, r)              -> let m1, l1 = typeCheckExpression types mem l
                               let m2, r1 = typeCheckExpression types m1 r
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> m2, BoolType None
                               | _                        -> Exception <| sprintf "type error with '%A' && '%A' " l r |> raise
  | Or (l, r)               -> let m1, l1 = typeCheckExpression types mem l
                               let m2, r1 = typeCheckExpression types m1 r
                               match (l1, r1) with
                               | (BoolType _, BoolType _) -> m2, BoolType None
                               | _                        -> Exception "type error with Or" |> raise
  | Condition (p, t, f)     -> let ifScope = narrowIfScope types <| Map.empty :: mem <| p
                               let _, ifT = typeCheckExpression types ifScope t
                               let m1 = ifScope.Tail
                               let m2, falseT = typeCheckExpression types m1 f
                               if isAssignable ifT falseT
                                 then m2, ifT
                                 elif isAssignable falseT ifT
                                 then m2, falseT
                                 else  Exception <| sprintf "type error in ? :, '%A' is not compatable with %A " ifT falseT |> raise
  | Nested e                -> typeCheckExpression types mem e
  | Equals (l ,r)           -> let m1, lt = typeCheckExpression types mem l
                               let m2, rt = typeCheckExpression types m1 r
                               match (lt, rt) with
                               | (BoolType _, BoolType _)       -> m2, BoolType None
                               | (StringType _, StringType _)   -> m2, BoolType None
                               | (IntType _, IntType _)         -> m2, BoolType None
                               | (UnitType _, UnitType _)       -> m2, BoolType None
                               | _                              -> Exception <| sprintf "Type error with '%A' == '%A'" l r |> raise
  | NotEquals (l, r)        -> let m1, lt = typeCheckExpression types mem l
                               let m2, rt = typeCheckExpression types m1 r
                               match (lt, rt) with
                               | (BoolType _, BoolType _)       -> m2, BoolType None
                               | (StringType _, StringType _)   -> m2, BoolType None
                               | (IntType _, IntType _)         -> m2, BoolType None
                               | (UnitType _, UnitType _)       -> m2, BoolType None
                               | _                              -> Exception <| sprintf "Type error with '%A' != '%A'" l r |> raise
  | Lambda (p, t, eb)          -> mem, FunctionType (typeToTypeEntry types t, typeCheckFuncBody types mem eb p <| typeToTypeEntry types t)
  | Apply (e, pe)           -> let m1, f = typeCheckExpression types mem e
                               let m2, pt = typeCheckExpression types m1 pe
                               match f with
                               | FunctionType (p, r) when isAssignable p pt -> m2, r
                               | FunctionType (p, _)                        -> Exception <| sprintf "Given value '%A' is not assignable to param '%A'" pt p |> raise
                               | _                                          -> Exception <| sprintf "No not callable:  '%A'" e |> raise
  | ArrayInit (i, t)        -> let t1 = typeToTypeEntry types t
                               List.map (fun e -> (let _, t2 = typeCheckExpression types mem e;
                                                if isAssignable t1 t2 |> not 
                                                then Exception <| sprintf "cannot assign '%A' to '%A'" t1 t |> raise)) i
                               mem, ArrayType t1
  | ArrayGet (a, i)         -> let m1, a1 = typeCheckExpression types mem a
                               let m2, i1 = typeCheckExpression types m1 i
                               match (a1, i1) with
                               | (ArrayType t, IntType _) -> m2, t
                               | (StringType _, IntType _) -> m2, StringType None
                               | _                        -> Exception "Cannot index '%A' with '%A'" |> raise
  | ObjectInit e            -> let exprs = Map.ofList e
                               mem, ObjectType <| Map.map (fun id e -> (let _, t = typeCheckExpression types mem e; 
                                                                      t)) exprs
  | ObjectGet (ob, id)      -> let m1, obj = typeCheckExpression types mem ob
                               match obj with
                               | ObjectType ps -> m1, ps.[id]
                               | _             -> Exception <| sprintf "'%A' is not an object" ob |> raise
  | ObjectCopyWith (ob, p)  -> let m1, obj = typeCheckExpression types mem ob
                               let id, v = p
                               let m2, valT = typeCheckExpression types m1 v
                               match obj with
                               | ObjectType ps when isAssignable ps.[id] valT -> m2, obj
                               | _                                            -> Exception <| sprintf "cannot assign prop '%s' on '%A' with '%A'" id obj valT |> raise
  | TypeAlias _             -> mem, UnitType
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
  | (ArrayType t1, ArrayType t2)                              -> isAssignable t1 t2
  | (ObjectType o1, ObjectType o2)                            -> let unmatched = Map.tryPick (fun id t1 -> (
                                                                                                if o2.ContainsKey id && isAssignable t1 o2.[id]
                                                                                                then None
                                                                                                else Some t1
                                                                                              )) o1
                                                                 unmatched.IsNone
  | _                                                         -> false

and narrowIfScope (types: Map<string, TypeEntry>) (mem: Memory<TypeEntry>) (pred: Expression) : Memory<TypeEntry> =
  match pred with
  | Equals (l, r) -> match (l, r) with
                     | (Value _, Read id) -> let _, t = typeCheckExpression types mem l
                                             writeMemory mem id t
                     | (Read id, Value _) -> let _, t = typeCheckExpression types mem r
                                             writeMemory mem id t
                     | _                  -> mem
  | And (l, r)    -> let m1 = narrowIfScope types mem l
                     narrowIfScope types m1 r
  | Nested e      -> narrowIfScope types mem e
  | _             -> mem

and typeToTypeEntry (types: Map<string, TypeEntry>) (t: Type): TypeEntry =
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
                           | _               -> types.[nt]
  | FuncType (p, r)     -> FunctionType (typeToTypeEntry types p, typeToTypeEntry types r)
  | NestedType t        -> typeToTypeEntry types t
  | Type.ArrayType t    -> ArrayType <| typeToTypeEntry types t
  | Type.ObjectType p   -> let props = Map.map (fun _ t -> typeToTypeEntry types t) <| Map.ofList p 
                           ObjectType props

and typeCheckFuncBody (types: Map<string, TypeEntry>) (mem: Memory<TypeEntry>) (exprs: Expression list) (paramAlias: string) (param: TypeEntry): TypeEntry =
  let mutable m1: Memory<TypeEntry> = Map.empty :: mem
  let mutable t : TypeEntry = UnitType
  m1 <- writeMemory mem paramAlias param
  List.map (fun e -> (let m2, t1 = typeCheckExpression types m1 e
                    m1 <- m2;
                    t <- t1)) exprs |> ignore
  t

let typeAliasses (exprs: Expression list) : Map<string, TypeEntry> = 
  List.filter (fun e -> (
    match e with
    | TypeAlias _ -> true
    | _           -> false
  )) exprs 
  |> List.map (fun e -> (
    match e with
    | TypeAlias (id, t) -> (id, typeToTypeEntry Map.empty t)
  ))
  |> Map.ofList

let typeCheckExpressions (exprs: Expression list) =
  let types = typeAliasses exprs
  let mutable mem: Memory<TypeEntry> = [Map.empty]
  List.map (fun e -> (let m1, _ = typeCheckExpression types mem e
                    mem <- m1;)) exprs |> ignore
