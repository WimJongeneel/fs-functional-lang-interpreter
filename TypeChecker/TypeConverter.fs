module TypeConverter

open System
open AST
open TypeCheckerState
open GenericTypes

let rec typeToTypeEntry (types: Memory<TypeEntry>) (t: Type): TypeEntry =
  match t with
  | LiteralType lt               -> match lt with
                                    | StringLiteral s -> StringType  <| Some s
                                    | IntLiteral i    -> IntType     <| Some i
                                    | BoolLiteral b   -> BoolType    <| Some b
                                    | UnitLiteral     -> UnitType
  | NamedType nt                 -> match nt with
                                    | "string"        -> StringType None
                                    | "int"           -> IntType None
                                    | "bool"          -> BoolType None
                                    | _               -> readMemory types nt
  | FuncType (p, g, r)           -> FunctionType (typeToTypeEntry types p, Map.map (fun _ t -> Option.map (typeToTypeEntry types) t) g, typeToTypeEntry types r)
  | NestedType t                 -> typeToTypeEntry types t
  | Type.ArrayType t             -> ArrayType <| typeToTypeEntry types t
  | Type.ObjectType p            -> Map.map (fun _ t -> typeToTypeEntry types t) <| Map.ofList p |> ObjectType
  | Type.UnionType cs            -> List.map (fun c -> typeToTypeEntry types c) cs |> UnionType
  | Type.ArgumentedType (a, i)   -> let typeArguments = Map.map (fun _ t -> Option.map (typeToTypeEntry types) t) a
                                    let localTypeScope = Map.map (fun id r -> GenericType (id, Option.map (typeToTypeEntry types) r)) a
                                    ArgumentedType (typeArguments, typeToTypeEntry <| localTypeScope :: types  <| i)
  | Type.AppliedType (pa, n)      -> let inner = readMemory types n
                                     match inner with 
                                     | ArgumentedType (ea, i) -> let localTypeScope = Map.toList ea
                                                                                    |> List.mapi (fun i t -> let id, _ = t in (id, pa.[i]))
                                                                                    |> Map.ofList
                                                                                    |> Map.map (fun _ t -> typeToTypeEntry types t)
                                                                 resolveGenerics i <| localTypeScope :: types
                                     | _                     -> Exception <| sprintf "'%A' is not an ArgumentedType" inner |> raise
