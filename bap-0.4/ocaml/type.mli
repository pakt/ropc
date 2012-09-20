(** Type declarations for BAP.
    
    @author Ivan Jager
*)

type label = 
  | Name of string (** For named labels*)
  | Addr of int64 (** For addresses. Cast REG_type as unsigned when comparing. *)


(** The IR type of a BAP expression *)
type typ =
  | Reg of int (** an N-bit bitvector (use 1 for booleans). Currently supported values: 1,8,16,32,64 *)
  | TMem of typ (** Memory of given index type and endianness*)
  | Array  of typ * typ (** Array of index type, element type. *)


(** Different forms of casting *)
type cast_type =
  | CAST_UNSIGNED (** 0-padding widening cast. *)
  | CAST_SIGNED (** Sign-extending widening cast. *)
  | CAST_HIGH (** Narrowning cast. Keeps the high bits. *)
  | CAST_LOW (** Narrowing cast. Keeps the low bits. *)

(** Binary operations implemented in the IR *)
type binop_type =
  | PLUS (** Integer addition. (commutative, associative) *)
  | MINUS (** Subtract second integer from first. *)
  | TIMES (** Integer multiplication. (commutative, associative)*)
  | DIVIDE (** Unsigned integer division. *)
  | SDIVIDE (** Signed integer division. *)
  | MOD (** Unsigned modulus. *)
  | SMOD (** Signed modulus. *)
  | LSHIFT (** Left shift. *)
  | RSHIFT (** Right shift, fill with 0. *)
  | ARSHIFT (** Right shift, sign extend. *)
  | AND (** Bitwise and. (commutative, associative) *)
  | OR (** Bitwise or. (commutative, associative) *)
  | XOR (** Bitwise xor. (commutative, associative) *)
  | EQ (** Equals (commutative) (associative on booleans) *)
  | NEQ (** Not equals (commutative) (associative on booleans) *)
  | LT (** Unsigned less than *)
  | LE (** Unsigned less than or equal to *)
  | SLT (** Signed less than *)
  | SLE (** Signed less than or equal to *)
                  

(** Unary operations implemented in the IR *)
type unop_type =
  | NEG (** Negate (2's complement) *)
  | NOT (** Bitwise not *)


(** The position of a statement in a source file *)
type pos = (string * int)

(** Extra attributes we can add to things.
    There may be a nicer way to implement this. Basically any extra information
    about a statement can be saved by wrapping the statement inside an Attr
    statement.
 *)

type taint_type = Taint of int
type usage = RD | WR | RW

type context = 
 {
   name  : string;
   mem   : bool;
   t     : typ;
   index : int64;
   value : Big_int.big_int;
   usage : usage;
   taint : taint_type
 }
 
type attribute = 
  | Pos of pos  (** The position of a statement in the source file *)
  | Asm of string
  | Address of int64
  | Liveout (** the variable assigned in this move should be considered live *)
  | StrAttr of string (** Generic printable and parseable attribute *)
  | Context of context         (** An attribute containing the concrete values
                                * and taint status of the instruction operands.
                                * It can be merged with `StrAttr' but it seems 
                                * more flexible to create a separate attribute. 
                                * - ethan *)
  | ThreadId of int
  | ExnAttr of exn (** Generic extensible attribute, but no parsing *)
  | InitRO (** The memory in this assignment is stored in the binary *)
  | Synthetic (** Operation was added by an analysis *)
type attributes = attribute list



(* stolen from Cil *)
(** Different visiting actions to be used by visitors. 'a will be
    instantiated with [exp], [stmt], etc. *)
type 'a visit_action = 
    [
    | `SkipChildren    (** Do not visit the children. Return the node
                           as it is. *)
    | `DoChildren      (** Continue with the children of this
                           node. Rebuild the node on return if any of
                           the children changes (use == test) *)
    | `ChangeTo of 'a  (** Replace the expression with the given
                           one *)
    | `ChangeToAndDoChildren of 'a (** Replace the expression with the
				   given one and do children  *)
(* FIXME: ChangeDoChildrenPost is fugly... find a better solution.
    | `DoChildrenPost of 'a -> 'a
	(** Continue on to the children, rebuild the node if changed, and apply
	the given function on the result. *)
    | `ChangeDoChildrenPost of 'a * ('a -> 'a)
        (** First consider that the entire 
			   exp is replaced by the first 
			   parameter. Then continue with 
			   the children. On return rebuild 
			   the node if any of the children 
			   has changed and then apply the 
			   function on the node *)
*)
    ]
