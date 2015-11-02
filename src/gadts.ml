(* ADTs *)
type value =
  | VBool of bool
  | VInt of int

type expr =
  | EValue of value
  | If of expr * expr * expr
  | Eq of expr * expr
  | Lt of expr * expr

let rec eval: expr -> value = function
  | EValue v -> v
  | Lt (x, y) -> begin match eval x, eval y with
      | VInt x, VInt y -> VBool (x < y)
      | VInt _, VBool _
      | VBool _, VInt _
      | VBool _, VBool _ -> failwith "Invalid AST"
    end
  | If (b, l, r) -> begin match eval b with
      | VBool true -> eval l
      | VBool false -> eval r
      | VInt _ -> failwith "Invalid AST"
    end
  | Eq (a, b) -> begin match eval a, eval b with
      | VInt  x, VInt  y -> VBool (x = y)
      | VBool _, VBool _
      | VBool _, VInt  _
      | VInt  _, VBool _ -> failwith "Invalid AST"
    end

let _ = eval (If ((Lt ((EValue (VInt 2)), (EValue (VInt 4)))),
                  (EValue (VInt 42)),
                  (EValue (VInt 0))))

let _ = eval (Eq ((EValue (VInt 42)), (EValue (VBool false))))

let eval_int: value -> int = function
  | VInt x -> x
  | VBool _ -> failwith "Got VBool, expected VInt"

let eval_bool: value -> bool = function
  | VBool b -> b
  | VInt _ -> failwith "Got VInt, expected VBool"

(* GADTs *)
type value' =
  | VBool' : bool -> value'
  | VInt' : int -> value'

type _ value' =
  | GBool : bool -> bool value'
  | GInt : int -> int value'

type _ expr' =
  | GValue : 'a value' -> 'a expr'
  | GIf : bool expr' * 'a expr' * 'a expr' -> 'a expr'
  | GEq : 'a expr' * 'a expr' -> bool expr'
  | GLt : int expr' * int expr' -> bool expr'

let rec eval' : type a. a expr' -> a = function
  | GValue (GBool b) -> b
  | GValue (GInt i) -> i
  | GIf (b, l, r) -> if eval' b then eval' l else eval' r
  | GEq (a, b) -> (eval' a) = (eval' b)
  | GLt (a,b) -> a < b

let _ =
  eval' (GIf ((GEq ((GValue (GInt 2)), (GValue (GInt 2)))),
              (GValue (GInt 42)),
              (GValue (GInt 12))));;

let _ =
  eval' (GIf (GInt 42, GInt 42, GInt 42));;

foo = GInt(42);;


