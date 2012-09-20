val replacer : ?opt:bool -> Cfg.SSA.G.t -> Cfg.SSA.G.t * bool
val aliased : Cfg.SSA.G.t -> Ssa.value -> Ssa.value -> bool option
