(* Simple sign analysis, based on 5.1 of the book *)

(* Lattice for description of the sign of an integer variable:
   Bottom — invalid state, i.e. no value, analog of NaN
   Top — any number, either positive or negative *)
type sign_lattice = Bottom | Positive | Negative | Zero | Top

let eval_binop (op : Anf.binop) (a : sign_lattice) (b : sign_lattice) =
  (* Processing only "real" numbers 9x9 lattice *)
  let plus a b =
    match (a, b) with
    | Positive, Positive -> Positive
    | Negative, Negative -> Negative
    | Zero, b -> b
    | a, Zero -> a
    | Positive, Negative -> Top
    | Negative, Positive -> Top
    | _, _ -> failwith "Unreachable in plus"
  in
  let minus a b =
    match (a, b) with
    | Positive, Positive -> Top
    | Negative, Negative -> Top
    | Zero, Positive -> Negative
    | Zero, Negative -> Positive
    | _, Zero -> a
    | Positive, Negative -> Positive
    | Negative, Positive -> Negative
    | _, _ -> failwith "Unreachable in minus"
  in
  let times a b =
    match (a, b) with
    | Zero, _ -> Zero
    | _, Zero -> Zero
    | Positive, Positive -> Positive
    | Negative, Negative -> Positive
    | Negative, Positive -> Negative
    | Positive, Negative -> Positive
    | _, _ -> failwith "Unreachable in times"
  in
  let div a b =
    match (a, b) with
    | Zero, _ -> Zero
    | _, Zero -> Bottom
    | Positive, Positive -> Positive
    | Positive, Negative -> Negative
    | Negative, Positive -> Negative
    | Negative, Negative -> Negative
    | _, _ -> failwith "Unreachable in div"
  in
  let greater a b =
    match (a, b) with
    | Positive, Positive -> Top
    | Positive, _ -> Positive
    | Negative, Negative -> Top
    | Negative, _ -> Zero
    | Zero, Negative -> Positive
    | Zero, _ -> Zero
    | _, _ -> failwith "Unreachable in greater"
  in
  let equal a b =
    match (a, b) with
    | Zero, Zero -> Positive
    | _, _ when a = b -> Top
    | _ -> Zero
  in
  match (a, b) with
  | Bottom, _ | _, Bottom -> Bottom
  | _ -> (
      match (op, a, b) with
      | Anf.Times, Zero, _ -> Zero
      | Anf.Times, _, Zero -> Zero
      | _, Top, _ -> Top
      | _, _, Top -> Top
      | Anf.Plus, _, _ -> plus a b
      | Anf.Minus, _, _ -> minus a b
      | Anf.Times, _, _ -> times a b
      | Anf.Div, _, _ -> div a b
      | Anf.Greater, _, _ -> greater a b
      | Anf.Equal, _, _ -> equal a b)

let analyze (_ : Typed_anf.program) = failwith "Unimplemented"
