module GenericTypes

open TypeCheckerState
open AST

let rec resolveGenerics (t: TypeEntry) (types: Memory<TypeEntry>): TypeEntry = 
  match t with 
  | GenericType (id, _)             -> let t1 = tryReadMemory types id in if t1.IsSome then t1.Value else t
  | FunctionType (p, g, r)          -> FunctionType (resolveGenerics p types, g, resolveGenerics r types)
  | TypeEntry.ArrayType i           -> TypeEntry.ArrayType <| resolveGenerics i types
  | TypeEntry.ObjectType o          -> TypeEntry.ObjectType <| Map.map (fun _ t -> resolveGenerics t types) o
  | TypeEntry.UnionType es          -> TypeEntry.UnionType <| List.map (fun t -> resolveGenerics t types) es
  | TypeEntry.ArgumentedType (a, i) -> match i with 
                                       | FunctionType (p, g, r) -> FunctionType (resolveGenerics p types, a, resolveGenerics r types)
                                       | _                      -> t
  | TypeEntry.ConditionalType (i, e, t, f) -> TypeEntry.ConditionalType (resolveGenerics i types, resolveGenerics e types, resolveGenerics t types, resolveGenerics f types)
  | _                               -> t
