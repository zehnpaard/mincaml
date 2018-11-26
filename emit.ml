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
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tst\t%s, [%s + %d]\n" x reg_sp (offset y)
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tstd\t%s, [%s + %d]\n" x reg_sp (offset y)
  | NonTail(_), Save(x, y) -> asssert (S.mem y !stackset); ()
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tstd\t%s, [%s + %d]\n" reg_sp (offset y) x
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tldd\t[%s + %d], %s\n" reg_sp (offset y) x
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | SLL _ | Ld _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _ as exp) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (Restore(x) as exp) ->
      (match locate x with
        | [i] -> g' oc (NonTail(regs.(0)), exp)
        | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
        | _ -> assert false);
  | Tail, IfEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tcmp\t%s, %s\n" x (pp_id_or_imm y);
      g'_tail_if oc e1 e2 "be" "bne"
  | Tail, IfLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tcmp\t%s, %s\n" x (pp_id_or_imm y);
      g'_tail_if oc e1 e2 "ble" "bg"
  | Tail, IfGE(x, y, e1, e2) ->
      Printf.fprintf oc "\tcmp\t%s, %s\n" x (pp_id_or_imm y);
      g'_tail_if oc e1 e2 "bge" "bl"
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpd\t%s, %s\n" x y;
      Printf.fprintf oc "\tnop\n";
      g'_tail_if oc e1 e2 "fbe" "fbne"
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpd\t%s, %s\n" x y;
      Printf.fprintf oc "\tnop\n";
      g'_tail_if oc e1 e2 "fble" "fbg"
  | NonTail(z), IfEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tcmp\t%s, %s\n" x (pp_id_or_imm y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "be" "bne"
  | NonTail(z), IfLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tcmp\t%s, %s\n" x (pp_id_or_imm y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bg"
  | NonTail(z), IfGE(x, y, e1, e2) ->
      Printf.fprintf oc "\tcmp\t%s, %s\n" x (pp_id_or_imm y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "bl"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpd\t%s, %s\n" x y;
      Printf.fprintf oc "\tnop\n";
      g'_non_non_tail_if oc (NonTail(z)) e1 e2 "fbe" "fbne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpd\t%s, %s\n" x y;
      Printf.fprintf oc "\tnop\n";
      g'_non_tail_if oc (NonTail(z)) e1 e2 "fble" "fbg"
  | Tail, CallCls(x, ys, zs) ->
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tld\t[%s + 0], %s\n" reg_cl reg_sw;
      Printf.fprintf oc "\tjmp\t%s\n" reg_sw;
      Printf.fprintf oc "\tnop\n"
  | Tail, CallDir(Id.L(x), ys, zs) ->
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tb\t%s\n" x;
      Printf.fprintf oc "\tnop\n"
  | NonTail(a), CallCls(x, ys, zs) ->
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tst\t%s, [%s + %d]\n" reg_ra reg_sp (ss - 4);
      Printf.fprintf oc "\tld\t[%s + 0], %s\n" reg_cl reg_sw;
      Printf.fprintf oc "\tcall\t%s\n" reg_sw;
      Printf.fprintf oc "\tadd\t%s, %d, %s\t! delay slot\n" reg_sp ss reg_sp;
      Printf.fprintf oc "\tsub\t%s, %d, %s\n" reg_sp ss reg_sp;
      Printf.fprintf oc "\tld\t[%s + %d], %s\n" reg_sp (ss - 4) reg_ra;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmov\t%s, %s\n" regs.(0) a
      else if List.mem a allfregs && a <> fregs.(0) then
        (Printf.fprintf oc "\tfmovs\t%s, %s\n" fregs.(0) a;
         Printf.fprintf oc "\tfmovs\t%s, %s\n" (co_freg fregs.(0)) (co_freg a))
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tst\t%s, [%s + %d]\n" reg_ra reg_sp (ss - 4);
      Printf.fprintf oc "\tcall\t%s\n" x;
      Printf.fprintf oc "\tadd\t%s, %d, %s\t! delay slot\n" reg_sp ss reg_sp;
      Printf.fprintf oc "\tsub\t%s, %d, %s\n" reg_sp ss reg_sp;
      Printf.fprintf oc "\tld\t[%s + %d], %s\n" reg_sp (ss - 4) reg_ra;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmov\t%s, %s\n" regs.(0) a
      else if List.mem a allfregs && a <> fregs.(0) then
        (Printf.fprintf oc "\tfmovs\t%s, %s\n" fregs.(0) a;
         Printf.fprintf oc "\tfmovs\t%s, %s\n" (co_freg fregs.(0)) (co_freg a))
