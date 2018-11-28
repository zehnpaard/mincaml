type id_or_imm = V of Id.t | C of int

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  | Set of int
  | Mov of Id.t
  | Sub of Id.t * id_or_imm
  | Comment of string
  | Save of Id.t * Id.t
  | Restore of Id.t

type prog = Prog of t

val regs : Id.t array
val allregs : Id.t list
val reg_cl : Id.t
val reg_sw : Id.t
val reg_fsw : Id.t
val reg_ra : Id.t
val reg_hp : Id.t
val reg_sp : Id.t
val is_reg : Id.t -> bool

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t

val align : int -> int
