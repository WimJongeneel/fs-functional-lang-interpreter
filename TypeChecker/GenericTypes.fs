module GenericTypes

open TypeCheckerState
open AST

let rec resolveGenerics (t: TypeEntry) (types: Memory<TypeEntry>): TypeEntry = 
  match t with 
  | GenericType (id, _)         -> readMemory types id
  | FunctionType (p, g, r)      -> FunctionType (resolveGenerics p types, g, resolveGenerics r types)
  | TypeEntry.ArrayType i       -> TypeEntry.ArrayType <| resolveGenerics i types
  | TypeEntry.ObjectType o      -> TypeEntry.ObjectType <| Map.map (fun _ t -> resolveGenerics t types) o
  | TypeEntry.UnionType es      -> TypeEntry.UnionType <| List.map (fun t -> resolveGenerics t types) es
  | _                           -> t
