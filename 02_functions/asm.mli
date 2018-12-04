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

val regs : Id.t array
val allregs : Id.t list
val reg_cl : Id.t
val reg_sw : Id.t
val reg_ra : Id.t
val reg_hp : Id.t
val reg_sp : Id.t
val is_reg : Id.t -> bool

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int
