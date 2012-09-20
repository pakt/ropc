val v2s : Cfg.AST.G.V.t -> string

val of_prog : ?special_error:bool -> Ast.program -> Cfg.AST.G.t

val to_prog : Cfg.AST.G.t -> Ast.program

val find_entry : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t
val find_error : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t
val find_exit : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t
val find_indirect : Cfg.AST.G.t -> Cfg.AST.G.t * Cfg.AST.G.V.t
