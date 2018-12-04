type id_or_imm = V of Id.t | C of int

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  | Nop
  | Set of int
  | SetL of Id.l
  | Mov of Id.t
  | Sub of Id.t * id_or_imm
  | SLL of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * Id.t * id_or_imm
  | Comment of string
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.t * Id.t list * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t

type fundef = { name : Id.l; 
                args : Id.t list; 
                fargs : Id.t list;
                body : t;
                ret : Type.t }

type prog = Prog of (Id.l * float) list * fundef list * t

let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs =
  [| "%i2"; "%i3"; "%i4"; "%i5";
     "%l0"; "%l1"; "%l2"; "%l3"; "%l4"; "%l5"; "%l6"; "%l7";
     "%o0"; "%o1"; "%o2"; "%o3"; "%o4"; "%o5" |]
let allregs = Array.to_list regs
let reg_cl = regs.(Array.length regs - 1)
let reg_sw = regs.(Array.length regs - 2)
let reg_sp = "%i0"
let reg_hp = "%i1"
let reg_ra = "%o7"
let is_reg x = (x.[0] = '%')

let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> remove_and_uniq (S.add x xs) ys

let fv_id_or_imm = function
  | V(x) -> [x]
  | _ -> []

let rec fv_exp = function
  | Nop
  | Set(_)
  | SetL(_)
  | Comment(_)
  | Restore(_) -> []
  | Mov(x)
  | Save(x, _) -> [x]
  | Sub(x, y')
  | SLL(x, y')
  | Ld(x, y') -> x :: fv_id_or_imm y'
  | St(x, y, z') -> x :: y :: fv_id_or_imm z'
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) -> fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
    | Ans(exp) -> Let(xt, exp, e2)
    | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)
