open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty
let stackmap = ref []
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs)
  in
  loc !stackmap
let offset x = 4 * List.hd (locate x)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> string_of_int i

type dest = Tail | NonTail of Id.t
let rec g oc = function
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function
  | NonTail(_), Nop -> ()
  | NonTail(x), Set(i) -> Printf.fprintf oc "\tset\t%d, %s\n" i x
  | NonTail(x), Mov(y) when x = y -> ()
  | NonTail(x), Mov(y) -> Printf.fprintf oc "\tmov\t%s, %s\n" y x
  | NonTail(x), Sub(y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" y (pp_id_or_imm z) x
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
  | Tail, (Nop | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Int), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (Set _ | Mov _ | Sub _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tret1\n";
      Printf.fprintf oc "\tnop\n"
  | Tail, (Restore(x) as exp) ->
      (match locate x with
        | [i] -> g' oc (NonTail(regs.(0)), exp)
        | _ -> assert false);

let f oc (Prog(e)) =
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
    Printf.fprintf oc ".global\tmin_caml_start\n";
    Printf.fprintf oc "min_caml_start:\n";
    Printf.fprintf oc "\tsave\t%%sp, -112, %%sp\n";
    stackset := S.empty;
    stackmap := [];
    g oc (NonTail("%g0"), e);
    Printf.fprintf oc "\tret\n";
    Printf.fprintf oc "\trestore\n";
