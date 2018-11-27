type t = string
type l = L of string

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
    incr counter;
    Printf.sprintf "%s.%d" s !counter

let rec id_of_typ = function
  | Type.Int -> "i"
  | Type.Var _ -> assert false
let gentmp typ =
    incr counter;
    Printf.sprintf "T%s%d" (id_of_typ typ) !counter
