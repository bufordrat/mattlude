(* helper code for reading configuration information out of the
   filesystem/shell environment *)

module Env = struct
  type (_,_) opt_or_def =
    | ReturnOption : ('a, 'a option) opt_or_def
    | DefaultTo : 'a -> ('a, 'a) opt_or_def

  let _lookup
          : type a. (string, a) opt_or_def -> string -> a =
    fun what_to_do str ->
    let open Prelude.Unix.Env in
    let value = env () |> List.assoc_opt str in
    match value with
    | Some v -> begin
        match what_to_do with
        | ReturnOption -> Some v
        | DefaultTo _ -> v
      end
    | None -> begin
        match what_to_do with
        | ReturnOption -> None
        | DefaultTo d -> d
      end

  (* let lookup  *)

  module type MONAD = Endofunctors_old.MONAD
  module R = Endofunctors_old.Result.Make (String)


(* let getenv = Prelude.Result.trapc
 *                "not in environment"
 *                Sys.getenv
 * let lookup env_vars =
 *   kleisli (List.map getenv env_vars) *)
  (* end *)
end
