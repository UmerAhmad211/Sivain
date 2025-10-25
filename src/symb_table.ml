open Ast

module Vars = struct
  type t = (string, dtype) Hashtbl.t list

  let create () : t = [ Hashtbl.create 4 ]
  let push_scope (env : t) : t = Hashtbl.create 4 :: env

  let add (env : t) (name : string) (dt : dtype) =
    match env with
    | [] -> Error "No current scope."
    | scope :: _ ->
        if Hashtbl.mem scope name then
          Error ("Duplicate variable in scope: " ^ name)
        else
          Ok (Hashtbl.add scope name dt)

  let rec find (env : t) (name : string) =
    match env with
    | [] -> Error ("Undeclared variable: " ^ name)
    | scope :: rest ->
        if Hashtbl.mem scope name then
          Ok (Hashtbl.find scope name)
        else
          find rest name

  let rec mem (env : t) (name : string) : bool =
    match env with
    | [] -> false
    | scope :: rest -> Hashtbl.mem scope name || mem rest name
end

module Funcs = struct
  type t = (string, dtype * (string * dtype) list) Hashtbl.t

  let create () : t = Hashtbl.create 4

  let add (env : t) (name : string) (rdt : dtype)
      (params : (string * dtype) list) =
    if Hashtbl.mem env name then
      Error ("Duplicate Function name " ^ name)
    else
      Ok (Hashtbl.add env name (rdt, params))

  let find (env : t) (name : string) : dtype * (string * dtype) list =
    Hashtbl.find env name

  let mem (env : t) (name : string) : bool = Hashtbl.mem env name
end

type env = {
  vars : Vars.t;
  fns : Funcs.t;
}

let creat_env () = { vars = Vars.create (); fns = Funcs.create () }
