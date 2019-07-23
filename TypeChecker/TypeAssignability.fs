module Typeassignability

open TypeCheckerState

let rec isAssignable (expected: TypeEntry) (given: TypeEntry) =
  match (expected, given) with
  | (StringType s1, StringType s2) when s1.IsSome && s2.IsSome -> s1.Value = s2.Value
  | (StringType s1, StringType _) when s1.IsNone              -> true
  | (IntType i1, IntType i2) when i1.IsSome && i2.IsSome       -> i1.Value = i2.Value
  | (IntType i1, IntType _) when i1.IsNone                    -> true
  | (BoolType b1, BoolType b2) when b1.IsSome && b2.IsSome     -> b1.Value = b2.Value
  | (BoolType b1, BoolType _) when b1.IsNone                  -> true
  | (UnitType, UnitType)                                      -> true
  | (FunctionType (p1, _, r1), FunctionType (p2, _, r2))      -> isAssignable p2 p1 && isAssignable r1 r2
  | (ArrayType t1, ArrayType t2)                              -> isAssignable t1 t2
  | (ObjectType o1, ObjectType o2)                            -> let unmatched = Map.tryPick (fun id t1 -> (
                                                                                                if o2.ContainsKey id && isAssignable t1 o2.[id]
                                                                                                then None
                                                                                                else Some t1
                                                                                              )) o1
                                                                 unmatched.IsNone
  | (_, UnionType cs2)                                        -> List.forall (fun c2 -> isAssignable expected c2) cs2
  | (UnionType cs, _)                                         -> List.exists (fun c -> isAssignable c given) cs
  // naive assumption that ids are unique
  | (GenericType (id1, _), GenericType (id2, _))              -> id1 = id2
  | (GenericType (_, r1), _)                                  -> let io = Option.map (fun c -> isAssignable c given) r1
                                                                 if io.IsSome && io.Value = true then true else false
  | (_, GenericType (_, r2))                                  -> let io = Option.map (fun c -> isAssignable c given) r2
                                                                 if io.IsSome && io.Value = true then true else false
  // Needs to check type restrictions on the type args
  | (ArgumentedType (a1, i1), ArgumentedType (a2, i2))        -> a1.Count = a2.Count && isAssignable i1 i2
  | (ArgumentedType (a1, i1), FunctionType (p, g, r))         -> match i1 with
                                                                 | FunctionType (ip, _, ir) -> g.Count = a1.Count && isAssignable p ip && isAssignable ir r
                                                                 | _                         -> false
  | _                                                         -> false