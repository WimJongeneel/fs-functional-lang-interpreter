module TypeCheckerState

open AST

type TypeEntry =
  | UnitType
  | IntType         of int option
  | BoolType        of bool option
  | StringType      of string option
  | FunctionType    of param: TypeEntry * generics: Map<string, (* restriction *) TypeEntry option> * ret: TypeEntry
  | ArrayType       of TypeEntry
  | ObjectType      of Map<string, TypeEntry>
  | UnionType       of TypeEntry list
  | GenericType     of id: string * restriction: TypeEntry option
  | ArgumentedType  of args: Map<string, TypeEntry option> * inner: TypeEntry
  | ConditionalType of input: TypeEntry * extends: TypeEntry * iftrue: TypeEntry * iffalse: TypeEntry

type TypeCheckerState = {
  vals: Memory<TypeEntry>
  types: Memory<TypeEntry>
}