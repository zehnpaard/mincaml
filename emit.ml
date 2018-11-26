open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty
let stackmap = ref []
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0
      then []
      else [Id.gentmp Type.Int]
    in
    stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs)
  in
  loc !stackmap
let offset x = 4 * List.hd (locate x)
let stacksize () = align ((List.length !stackmap + 1) * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> string_of_int i

let rec shuffle sw xys =
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
    | [], [] -> []
    | (x, y) :: xys, [] ->
        let f = function
          | (y', z) when y' = y -> (sw, z)
          | yz -> yz
        in
        (y, sw) :: (x, y) :: shuffle sw (List.map f xys)
    | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t
let rec g oc = function
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function
  | NonTail(_), Nop -> ()
  | NonTail(x), Set(i) -> Printf.fprintf oc "\tset\t%d, %s\n" i x
  | NonTail(x), SetL(Id.L(y)) -> Printf.fprintf oc "\tset\t%d, %s\n" y x
  | NonTail(x), Mov(y) when x = y -> ()
  | NonTail(x), Mov(y) -> Printf.fprintf oc "\tmov\t%s, %s\n" y x
  | NonTail(x), Neg(y) -> Printf.fprintf oc "\tneg\t%s, %s\n" y x
  | NonTail(x), Add(y, z) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" y (pp_id_or_imm z) x
  | NonTail(x), Sub(y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" y (pp_id_or_imm z) x
  | NonTail(x), SLL(y, z) -> Printf.fprintf oc "\tsll\t%s, %s, %s\n" y (pp_id_or_imm z) x
  | NonTail(x), Ld(y, z) -> Printf.fprintf oc "\tld\t[%s + %s], %s\n" y (pp_id_or_imm z) x
  | NonTail(_), St(x, y, z) -> Printf.fprintf oc "\tst\t%s, [%s  + %s]\n" x y (pp_id_or_imm z)
  | NonTail(x), FMovD(y) when x = y -> ()
  | NonTail(x), FMovD(y) ->
      Printf.fprintf oc "\tfmovs\t%s, %s\n" y x;
      Printf.fprintf oc "\tfmovs\t%s, %s\n" (co_freg y) (co_freg x)
  | NonTail(x), FNegD(y) ->
      Printf.fprintf oc "\tfnegs\t%s, %s\n" y x;
      if x <> y then Printf.fprint oc "\tmovs\t%s, %s\n" (co_freg y) (co_freg x)
  | NonTail(x), FAddD(y, z) -> Printf.fprintf oc "\tfaddd\t%s, %s, %s\n" y z x
  | NonTail(x), FSubD(y, z) -> Printf.fprintf oc "\tfsubd\t%s, %s, %s\n" y z x
  | NonTail(x), FMulD(y, z) -> Printf.fprintf oc "\tfmuld\t%s, %s, %s\n" y z x
  | NonTail(x), FDivD(y, z) -> Printf.fprintf oc "\tfdivd\t%s, %s, %s\n" y z x
  | NonTail(x), LdDF(y, z) -> Printf.fprintf oc "\tldd\t[%s + %s], %s\n" y (pp_id_or_imm z) x
  | NonTail(_), StDF(x, y, z) -> Printf.fprintf oc "\tstd\t%s, [%s + %s]\n" x y (pp_id_or_imm z)
  | NonTail(_), Comment(s) -> Printf.fprintf oc "\t! %s\n" s
