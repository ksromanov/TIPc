(* Reveal busy expressions pass. In contrast to available expressions
   pass we are storing data for each AST node in a Hashmap *)
(* Temporary storage of available expressions with printing *)
module S_expressions = struct
  include Set.Make (struct
    let compare = Stdlib.compare

    type t = Anf.complex_expression
  end)

  let pp fmt s =
    Format.fprintf fmt "%s" @@ [%show: Anf.complex_expression list] (elements s)
end

(* Remove all expressions, containing specific variable *)
let filter_out_variable id =
  S_expressions.filter (fun expr ->
      match expr with
      | Binop (Id a, _, Id b) when id = a || id = b -> true
      | _ -> false)

(* Resulting type of the expression *)
type busy_t = Busy | Regular [@@deriving show]
type payload = busy_t * S_expressions.t [@@deriving show]

type statement = Single of Anf.statement * payload | Block of statement list
[@@deriving show]

type func = {
  name : string;
  args : Anf.argument list;
  var_blocks : string list list;
  stmts : statement list;
  ret_expr : Anf.atomic_expression;
}
[@@deriving show]

(* intermediate data of the pass *)
type very_busy_expressions_t =
  (Anf.complex_expression, S_expressions.t) Hashtbl.t

let analyze_function (_ : Typed_anf.func) : very_busy_expressions_t =
  let analyze_statement = failwith "" in
  Hashtbl.create 10

let analyze (program : Typed_anf.program) : very_busy_expressions_t list =
  List.map analyze_function program
