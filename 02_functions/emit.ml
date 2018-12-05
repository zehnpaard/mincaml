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
  | NonTail(x), Sub(y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" y (pp_id_or_imm z) x
  | NonTail(x), SLL(y, z) -> Printf.fprintf oc "\tsll\t%s, %s, %s\n" y (pp_id_or_imm z) x
  | NonTail(x), Ld(y, z) -> Printf.fprintf oc "\tld\t[%s + %s], %s\n" y (pp_id_or_imm z) x
  | NonTail(_), St(x, y, z) -> Printf.fprintf oc "\tst\t%s, [%s  + %s]\n" x y (pp_id_or_imm z)
  | NonTail(_), Comment(s) -> Printf.fprintf oc "\t! %s\n" s
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tst\t%s, [%s + %d]\n" x reg_sp (offset y)
  | NonTail(_), Save(x, y) -> asssert (S.mem y !stackset); ()
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tstd\t%s, [%s + %d]\n" reg_sp (offset y) x
  | Tail, (Nop | St _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (Set _ | SetL _ | Mov _ | Sub _ | SLL _ | Ld _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (Restore(x) as exp) ->
      (match locate x with
        | [i] -> g' oc (NonTail(regs.(0)), exp)
        | _ -> assert false);
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
and g'_args oc x_reg_cl ys zs =
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys
  in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tmov\t%s, %s\n" y r)
    (shuffle reg_sw yrs);

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generationg assembly...@.";
  Printf.fprintf oc ".section\t\".rodata\"\n";
  Printf.fprintf oc ".align\t8\n";
  List.iter
    (fun (Id.L(x), d) ->
      Printf.fprintf oc "%s:\t! %f\n" x d;
      Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
      Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d))
    data;
    Printf.fprintf oc ".section\t\".text\"\n";
    List.iter (fun fundef -> h oc fundef) fundefs;
    Printf.fprintf oc ".global\tmin_caml_start\n";
    Printf.fprintf oc "min_caml_start:\n";
    Printf.fprintf oc "\tsave\t%%sp, -112, %%sp\n";
    stackset := S.empty;
    stackmap := [];
    g oc (NonTail("%g0"), e);
    Printf.fprintf oc "\tret\n";
    Printf.fprintf oc "\trestore\n";
