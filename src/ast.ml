type dtype =
  | Dint
  | Dvoid

type exp =
  | Int of int
  | Ret of exp

type func = {
  name : string;
  params : (string * dtype) list;
  ret_type : dtype;
  body : exp list;
}

type prog = func list
