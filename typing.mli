exception Error of Syntax.t * Type.t * Type.t
val extenv : Type.t M.t ref
val ret_type : Type.t ref
val f : Syntax.t -> Syntax.t
