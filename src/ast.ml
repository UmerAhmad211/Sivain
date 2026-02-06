type pos = Lexing.position
type dtype = Dint | Dvoid | Dfloat

type binop =
  | Eq
  | Neq
  | Les
  | Grt
  | Lese
  | Grte
  | Add
  | Mul
  | Sub
  | Div
  | Mod
  | And
  | Or

type uop = Not | Neg | Til

type expr =
  | Int of pos * int
  | Float of pos * float
  | Id_name of pos * string
  | Binop of pos * binop * expr * expr
  | Uop of pos * uop * expr
  | Call of pos * string * expr list
  | Assign of pos * expr * expr

type stmt =
  | Ret of pos * expr option
  | Expr of pos * expr
  | Decl of pos * string * dtype * expr option
  | If of pos * expr * stmt list * stmt list option
  | While of pos * expr * stmt list
  | Block of pos * stmt list

type func = {
  name : pos * string;
  params : (string * dtype * pos) list;
  ret_type : pos * dtype;
  body : stmt list;
}

type top_level_decls =
  | Funcs of func
  | Global_Vars of pos * string * dtype * expr option

type prog = top_level_decls list
